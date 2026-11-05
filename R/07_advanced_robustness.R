in_overlap <- file.path("outputs","audit","wcad_usda_overlap","wcad_usda_overlap_by_census.csv")
in_rental <- file.path("outputs","audit","wcad","rental_observations.csv")
in_base <- file.path("outputs","audit","final_sample","analytical_sample_baseline.csv")
out_dir <- file.path("outputs","models","advanced")
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
for (f in c(in_overlap,in_rental,in_base)) if (!file.exists(f)) stop("Missing input: ", f)

overlap <- read.csv(in_overlap, stringsAsFactors=FALSE, check.names=FALSE)
rental <- read.csv(in_rental, stringsAsFactors=FALSE, check.names=FALSE)
base <- read.csv(in_base, stringsAsFactors=FALSE, check.names=FALSE)

hc3 <- function(model, name) {
  X <- model.matrix(model); b <- coef(model); keep <- !is.na(b); X <- X[,keep,drop=FALSE]; b <- b[keep]
  e <- residuals(model); h <- hatvalues(model); bread <- solve(crossprod(X)); w <- (e/(1-h))^2
  V <- bread %*% crossprod(X, X*w) %*% bread; se <- sqrt(diag(V)); df <- df.residual(model)
  data.frame(model=name, term=names(b), estimate=unname(b), std_error=unname(se), statistic=unname(b/se),
             p_value=2*pt(abs(b/se),df=df,lower.tail=FALSE), n=nobs(model), clusters=NA_integer_, stringsAsFactors=FALSE)
}

cluster_cr1 <- function(model, cluster, name) {
  X0 <- model.matrix(model); b0 <- coef(model); keep <- !is.na(b0); X <- X0[,keep,drop=FALSE]; b <- b0[keep]
  e <- residuals(model); cl <- as.character(cluster); G <- length(unique(cl)); N <- nrow(X); K <- ncol(X)
  bread <- solve(crossprod(X)); meat <- matrix(0,K,K)
  for (g in unique(cl)) { idx <- cl==g; s <- crossprod(X[idx,,drop=FALSE], e[idx]); meat <- meat + s %*% t(s) }
  adj <- (G/(G-1))*((N-1)/(N-K)); V <- adj * bread %*% meat %*% bread; se <- sqrt(diag(V)); df <- G-1
  data.frame(model=name, term=names(b), estimate=unname(b), std_error=unname(se), statistic=unname(b/se),
             p_value=2*pt(abs(b/se),df=df,lower.tail=FALSE), n=N, clusters=G, stringsAsFactors=FALSE)
}

# 1) Within-country repeated-census model, all eligible post-2000 censuses.
overlap$census_year <- as.integer(overlap$census_year); overlap$rental_area_share <- as.numeric(overlap$rental_area_share)
overlap$tfp_growth_post5 <- as.numeric(overlap$tfp_growth_post5)
panel <- overlap[overlap$census_year>=2000 & overlap$post5_available %in% c(TRUE,"TRUE") & complete.cases(overlap[,c("wcad_country","rental_area_share","tfp_growth_post5")]),,drop=FALSE]
counts <- table(panel$wcad_country); panel <- panel[panel$wcad_country %in% names(counts[counts>=2]),,drop=FALSE]
panel_countries <- length(unique(panel$wcad_country))
if (panel_countries >= 10) {
  panel$decade <- factor(floor(panel$census_year/10)*10)
  m_fe <- lm(tfp_growth_post5 ~ rental_area_share + factor(wcad_country) + decade, data=panel)
  r_fe <- cluster_cr1(m_fe, panel$wcad_country, "country_fe_repeated_censuses")
} else {
  r_fe <- data.frame(model="country_fe_repeated_censuses",term="rental_area_share",estimate=NA_real_,std_error=NA_real_,statistic=NA_real_,p_value=NA_real_,n=nrow(panel),clusters=panel_countries,stringsAsFactors=FALSE)
}

# 2) Alternative exposure: percentage of holdings rented, same baseline country-year.
hold <- rental[tolower(trimws(rental$unit))=="%" & rental$measure_dimension=="holdings" & !is.na(rental$value), c("country","census_year","value"),drop=FALSE]
hold$census_year <- as.integer(hold$census_year); hold$value <- as.numeric(hold$value)
key <- paste(hold$country,hold$census_year,sep="\u001f"); sp <- split(hold$value,key)
conf <- vapply(sp,function(x) length(unique(x))>1,logical(1)); if(any(conf)) stop("Conflicting holding-share values for country-year.")
hold <- hold[!duplicated(key),,drop=FALSE]; names(hold) <- c("wcad_country","census_year","rental_holding_share")
alt <- merge(base,hold,by=c("wcad_country","census_year"),all.x=FALSE,all.y=FALSE)
for(v in c("tfp_growth_post5","rental_holding_share","average_holding_area_ha","log_gdp_per_capita","agriculture_value_added_pct_gdp")) alt[[v]] <- as.numeric(alt[[v]])
alt$log_average_holding_area <- log(alt$average_holding_area_ha)
alt <- alt[complete.cases(alt[,c("tfp_growth_post5","rental_holding_share","log_average_holding_area","log_gdp_per_capita","agriculture_value_added_pct_gdp")]),,drop=FALSE]
if(nrow(alt)<40) stop("Alternative holding-share sample is too small: ",nrow(alt))
m_alt <- lm(tfp_growth_post5 ~ rental_holding_share + log_average_holding_area + log_gdp_per_capita + agriculture_value_added_pct_gdp, data=alt)
r_alt <- hc3(m_alt,"alternative_rental_holding_share")

# 3) Acceleration: post-census minus pre-census 5-year TFP growth.
acc <- base
for(v in c("tfp_growth_post5","tfp_growth_pre5","rental_area_share","average_holding_area_ha","log_gdp_per_capita","agriculture_value_added_pct_gdp")) acc[[v]] <- suppressWarnings(as.numeric(acc[[v]]))
acc$log_average_holding_area <- log(acc$average_holding_area_ha); acc$tfp_growth_acceleration <- acc$tfp_growth_post5-acc$tfp_growth_pre5
acc <- acc[complete.cases(acc[,c("tfp_growth_acceleration","rental_area_share","log_average_holding_area","log_gdp_per_capita","agriculture_value_added_pct_gdp")]),,drop=FALSE]
m_acc <- lm(tfp_growth_acceleration ~ rental_area_share + log_average_holding_area + log_gdp_per_capita + agriculture_value_added_pct_gdp, data=acc)
r_acc <- hc3(m_acc,"tfp_growth_acceleration")

res <- rbind(r_fe,r_alt,r_acc)
write.csv(res,file.path(out_dir,"advanced_robustness_results.csv"),row.names=FALSE,na="")
focus <- res[res$term %in% c("rental_area_share","rental_holding_share"),,drop=FALSE]
focus$estimate_per_10pp <- 10*focus$estimate
write.csv(focus,file.path(out_dir,"advanced_rental_effects.csv"),row.names=FALSE,na="")
summary <- data.frame(test=c("country_fe_repeated_censuses","alternative_rental_holding_share","tfp_growth_acceleration"),
                      n=c(nrow(panel),nrow(alt),nrow(acc)), countries=c(panel_countries,nrow(alt),nrow(acc)), stringsAsFactors=FALSE)
write.csv(summary,file.path(out_dir,"advanced_sample_summary.csv"),row.names=FALSE,na="")
cat("ADVANCED_ROBUSTNESS_OK\n"); print(summary,row.names=FALSE); cat("\nRental effects:\n"); print(focus,row.names=FALSE)
