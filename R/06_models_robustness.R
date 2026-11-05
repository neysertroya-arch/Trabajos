in_baseline <- file.path("outputs", "audit", "final_sample", "analytical_sample_baseline.csv")
in_structural <- file.path("outputs", "audit", "final_sample", "analytical_sample_structural.csv")
out_dir <- file.path("outputs", "models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (f in c(in_baseline, in_structural)) if (!file.exists(f)) stop("Required input missing: ", f)

baseline <- read.csv(in_baseline, stringsAsFactors = FALSE, check.names = FALSE)
structural <- read.csv(in_structural, stringsAsFactors = FALSE, check.names = FALSE)

hc3_tidy <- function(model, model_name) {
  X <- stats::model.matrix(model)
  e <- stats::residuals(model)
  h <- stats::hatvalues(model)
  bread <- solve(crossprod(X))
  w <- (e / (1 - h))^2
  meat <- crossprod(X, X * w)
  V <- bread %*% meat %*% bread
  se <- sqrt(diag(V))
  b <- stats::coef(model)
  df <- stats::df.residual(model)
  tval <- b / se
  pval <- 2 * stats::pt(abs(tval), df = df, lower.tail = FALSE)
  crit <- stats::qt(0.975, df = df)
  data.frame(
    model = model_name,
    term = names(b),
    estimate = unname(b),
    std_error_hc3 = unname(se),
    statistic = unname(tval),
    p_value = unname(pval),
    conf_low_95 = unname(b - crit * se),
    conf_high_95 = unname(b + crit * se),
    n = stats::nobs(model),
    stringsAsFactors = FALSE
  )
}

prep_common <- function(d) {
  numeric_vars <- c("rental_area_share", "tfp_growth_post5", "tfp_growth_post3", "tfp_growth_pre5",
                    "average_holding_area_ha", "log_gdp_per_capita", "agriculture_value_added_pct_gdp")
  for (v in intersect(numeric_vars, names(d))) d[[v]] <- suppressWarnings(as.numeric(d[[v]]))
  if (any(d$average_holding_area_ha <= 0, na.rm = TRUE)) stop("Average holding area must be positive.")
  d$log_average_holding_area <- log(d$average_holding_area_ha)
  d
}

baseline <- prep_common(baseline)
structural <- prep_common(structural)

main_vars <- c("tfp_growth_post5", "rental_area_share", "log_average_holding_area", "log_gdp_per_capita", "agriculture_value_added_pct_gdp")
if (any(!complete.cases(baseline[, main_vars]))) stop("Main sample contains missing model values.")

m_main <- stats::lm(tfp_growth_post5 ~ rental_area_share + log_average_holding_area + log_gdp_per_capita + agriculture_value_added_pct_gdp, data = baseline)

structural$small_holdings_share_lt2 <- suppressWarnings(as.numeric(structural$small_holdings_share_lt2))
structural$rental_c <- structural$rental_area_share - mean(structural$rental_area_share)
structural$small_c <- structural$small_holdings_share_lt2 - mean(structural$small_holdings_share_lt2)
m_structural <- stats::lm(tfp_growth_post5 ~ rental_c * small_c + log_average_holding_area + log_gdp_per_capita + agriculture_value_added_pct_gdp, data = structural)

# Predefined robustness 1: 3-year post-census TFP growth, same baseline countries/census years.
r3 <- baseline[complete.cases(baseline[, c("tfp_growth_post3", "rental_area_share", "log_average_holding_area", "log_gdp_per_capita", "agriculture_value_added_pct_gdp")]), , drop = FALSE]
m_post3 <- stats::lm(tfp_growth_post3 ~ rental_area_share + log_average_holding_area + log_gdp_per_capita + agriculture_value_added_pct_gdp, data = r3)

# Predefined robustness 2: pre-census 5-year TFP growth as an outcome/placebo-style diagnostic, not a causal test.
rpre <- baseline[complete.cases(baseline[, c("tfp_growth_pre5", "rental_area_share", "log_average_holding_area", "log_gdp_per_capita", "agriculture_value_added_pct_gdp")]), , drop = FALSE]
m_pre5 <- stats::lm(tfp_growth_pre5 ~ rental_area_share + log_average_holding_area + log_gdp_per_capita + agriculture_value_added_pct_gdp, data = rpre)

# Predefined robustness 3: remove only observations flagged as influential by Cook's D > 4/N in the main model.
cook <- stats::cooks.distance(m_main)
influential <- cook > (4 / nrow(baseline))
sens <- baseline[!influential, , drop = FALSE]
m_no_influential <- stats::lm(tfp_growth_post5 ~ rental_area_share + log_average_holding_area + log_gdp_per_capita + agriculture_value_added_pct_gdp, data = sens)

results <- do.call(rbind, list(
  hc3_tidy(m_main, "main_post5"),
  hc3_tidy(m_structural, "structural_interaction"),
  hc3_tidy(m_post3, "robust_post3"),
  hc3_tidy(m_pre5, "diagnostic_pre5"),
  hc3_tidy(m_no_influential, "robust_no_influential")
))
write.csv(results, file.path(out_dir, "model_results_hc3.csv"), row.names = FALSE, na = "")

rental_terms <- results[results$term %in% c("rental_area_share", "rental_c", "rental_c:small_c"), , drop = FALSE]
rental_terms$estimate_per_10pp <- ifelse(rental_terms$term %in% c("rental_area_share", "rental_c"), 10 * rental_terms$estimate, NA_real_)
rental_terms$conf_low_95_per_10pp <- ifelse(rental_terms$term %in% c("rental_area_share", "rental_c"), 10 * rental_terms$conf_low_95, NA_real_)
rental_terms$conf_high_95_per_10pp <- ifelse(rental_terms$term %in% c("rental_area_share", "rental_c"), 10 * rental_terms$conf_high_95, NA_real_)
write.csv(rental_terms, file.path(out_dir, "rental_effect_summary.csv"), row.names = FALSE, na = "")

influence_table <- data.frame(
  wcad_country = baseline$wcad_country,
  census_year = baseline$census_year,
  cooks_distance = cook,
  influential_4_over_n = influential,
  stringsAsFactors = FALSE
)
write.csv(influence_table, file.path(out_dir, "main_model_influence.csv"), row.names = FALSE, na = "")

model_summary <- data.frame(
  model = c("main_post5", "structural_interaction", "robust_post3", "diagnostic_pre5", "robust_no_influential"),
  n = c(stats::nobs(m_main), stats::nobs(m_structural), stats::nobs(m_post3), stats::nobs(m_pre5), stats::nobs(m_no_influential)),
  r_squared = c(summary(m_main)$r.squared, summary(m_structural)$r.squared, summary(m_post3)$r.squared, summary(m_pre5)$r.squared, summary(m_no_influential)$r.squared),
  adjusted_r_squared = c(summary(m_main)$adj.r.squared, summary(m_structural)$adj.r.squared, summary(m_post3)$adj.r.squared, summary(m_pre5)$adj.r.squared, summary(m_no_influential)$adj.r.squared),
  stringsAsFactors = FALSE
)
write.csv(model_summary, file.path(out_dir, "model_summary.csv"), row.names = FALSE, na = "")

cat("MAIN_MODELS_ROBUSTNESS_OK\n")
cat("Influential observations removed in sensitivity:", sum(influential), "\n")
cat("\nRental-related HC3 results:\n")
print(rental_terms, row.names = FALSE)
cat("\nModel summary:\n")
print(model_summary, row.names = FALSE)
