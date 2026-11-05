# 08_publication_outputs.R -------------------------------------------------
# Build publication-ready tables, figures, and a generated Results fragment
# directly from approved analytical outputs. No inferential model is re-estimated here.

out_dir <- file.path("outputs", "publication")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

files <- list(
  sample = file.path("outputs", "audit", "final_sample", "analytical_sample_baseline.csv"),
  desc = file.path("outputs", "diagnostics", "descriptive_statistics.csv"),
  models = file.path("outputs", "models", "model_results_hc3.csv"),
  rental = file.path("outputs", "models", "rental_effect_summary.csv"),
  advanced = file.path("outputs", "models", "advanced", "advanced_rental_effects.csv"),
  advanced_n = file.path("outputs", "models", "advanced", "advanced_sample_summary.csv")
)
for (f in unlist(files)) if (!file.exists(f)) stop("Missing approved analytical output: ", f)

d <- read.csv(files$sample, stringsAsFactors = FALSE, check.names = FALSE)
desc <- read.csv(files$desc, stringsAsFactors = FALSE, check.names = FALSE)
models <- read.csv(files$models, stringsAsFactors = FALSE, check.names = FALSE)
rental <- read.csv(files$rental, stringsAsFactors = FALSE, check.names = FALSE)
advanced <- read.csv(files$advanced, stringsAsFactors = FALSE, check.names = FALSE)
advanced_n <- read.csv(files$advanced_n, stringsAsFactors = FALSE, check.names = FALSE)

fmt <- function(x, digits = 3) formatC(as.numeric(x), format = "f", digits = digits)
fmtp <- function(x) {
  x <- as.numeric(x)
  ifelse(x < .001, "<0.001", formatC(x, format = "f", digits = 3))
}
md_table <- function(df, path, align = NULL, caption = NULL) {
  x <- as.data.frame(df, stringsAsFactors = FALSE)
  x[] <- lapply(x, as.character)
  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  if (is.null(align)) align <- rep("---", ncol(x))
  rule <- paste0("| ", paste(align, collapse = " | "), " |")
  rows <- apply(x, 1, function(z) paste0("| ", paste(z, collapse = " | "), " |"))
  out <- c()
  if (!is.null(caption)) out <- c(out, paste0("*", caption, "*"), "")
  out <- c(out, header, rule, rows)
  writeLines(out, path, useBytes = TRUE)
}

# Table 1: core descriptive statistics.
labels <- c(
  tfp_growth_post5 = "TFP growth, post-census 5-year annualised log change",
  rental_area_share = "Agricultural area rented from others (%)",
  average_holding_area_ha = "Average agricultural holding area (ha)",
  log_average_holding_area = "Log average holding area",
  log_gdp_per_capita = "Log GDP per capita",
  agriculture_value_added_pct_gdp = "Agriculture value added (% GDP)"
)
t1 <- desc[desc$variable %in% names(labels), c("variable","n","mean","sd","min","median","max"), drop = FALSE]
t1$Variable <- unname(labels[t1$variable])
t1 <- t1[, c("Variable","n","mean","sd","min","median","max")]
names(t1) <- c("Variable","N","Mean","SD","Min","Median","Max")
for (v in c("Mean","SD","Min","Median","Max")) t1[[v]] <- fmt(t1[[v]], 3)
md_table(
  t1,
  file.path(out_dir, "table1_descriptives.md"),
  c("---","---:","---:","---:","---:","---:","---:"),
  "Table 1. Descriptive statistics for the baseline analytical sample"
)
write.csv(t1, file.path(out_dir, "table1_descriptives.csv"), row.names = FALSE)

# Table 2: approved main and structural models, HC3 inference.
keep_models <- c("main_post5", "structural_interaction")
keep_terms <- c("rental_area_share","rental_c","small_c","rental_c:small_c","log_average_holding_area","log_gdp_per_capita","agriculture_value_added_pct_gdp")
t2 <- models[models$model %in% keep_models & models$term %in% keep_terms, c("model","term","estimate","std_error_hc3","p_value","n"), drop = FALSE]
model_labels <- c(main_post5="Main: post-census TFP growth", structural_interaction="Farm-structure interaction")
term_labels <- c(
  rental_area_share="Rental-area share",
  rental_c="Rental-area share (centred)",
  small_c="Small-holding share <2 ha (centred)",
  `rental_c:small_c`="Rental × small-holding share",
  log_average_holding_area="Log average holding area",
  log_gdp_per_capita="Log GDP per capita",
  agriculture_value_added_pct_gdp="Agriculture value added (% GDP)"
)
t2$Model <- unname(model_labels[t2$model]); t2$Term <- unname(term_labels[t2$term])
t2$Estimate <- fmt(t2$estimate, 5); t2$`HC3 SE` <- fmt(t2$std_error_hc3, 5); t2$p <- fmtp(t2$p_value); t2$N <- t2$n
t2 <- t2[, c("Model","Term","Estimate","HC3 SE","p","N")]
md_table(
  t2,
  file.path(out_dir, "table2_main_models.md"),
  c("---","---","---:","---:","---:","---:"),
  "Table 2. Main cross-country models with HC3 robust inference"
)
write.csv(t2, file.path(out_dir, "table2_main_models.csv"), row.names = FALSE)

# Table 3: robustness focused only on interpretable rental main effects.
r1 <- rental[rental$term %in% c("rental_area_share", "rental_c"), c("model","term","estimate_per_10pp","p_value","n"), drop = FALSE]
r1$test <- r1$model
r1$effect10 <- r1$estimate_per_10pp
r1$p <- r1$p_value
r1$N <- r1$n
r2 <- advanced[, c("model","term","estimate_per_10pp","p_value","n"), drop = FALSE]
r2$test <- r2$model
r2$effect10 <- r2$estimate_per_10pp
r2$p <- r2$p_value
r2$N <- r2$n
rob <- rbind(r1[,c("test","term","effect10","p","N")], r2[,c("test","term","effect10","p","N")])
rob_labels <- c(
  main_post5="Main post-5-year model",
  structural_interaction="Structural interaction: rental main effect",
  robust_post3="Post-3-year TFP growth",
  diagnostic_pre5="Pre-census 5-year TFP growth",
  robust_no_influential="Excluding influential observations",
  country_fe_repeated_censuses="Country FE, repeated censuses",
  alternative_rental_holding_share="Alternative: rented holdings share",
  tfp_growth_acceleration="TFP acceleration: post minus pre"
)
rob$Test <- ifelse(rob$test %in% names(rob_labels), unname(rob_labels[rob$test]), rob$test)
rob$`Effect per 10 pp` <- fmt(rob$effect10, 4); rob$p <- fmtp(rob$p); rob$N <- as.integer(rob$N)
rob <- rob[, c("Test","Effect per 10 pp","p","N")]
md_table(
  rob,
  file.path(out_dir, "table3_robustness.md"),
  c("---","---:","---:","---:"),
  "Table 3. Rental-related coefficients across predefined robustness checks"
)
write.csv(rob, file.path(out_dir, "table3_robustness.csv"), row.names = FALSE)

# Numeric types and complete samples used only for publication figures.
num_vars <- c("rental_area_share", "tfp_growth_post5", "average_holding_area_ha",
              "log_gdp_per_capita", "agriculture_value_added_pct_gdp")
for (v in num_vars) d[[v]] <- suppressWarnings(as.numeric(d[[v]]))
d$log_average_holding_area <- log(d$average_holding_area_ha)

# Figure 1: rental prevalence vs post-census TFP growth.
d_fig1 <- d[complete.cases(d[, c("rental_area_share", "tfp_growth_post5")]), , drop = FALSE]
png(file.path(out_dir, "figure1_rental_tfp.png"), width = 1800, height = 1200, res = 180)
plot(d_fig1$rental_area_share, d_fig1$tfp_growth_post5,
     xlab = "Agricultural area rented from others (%)",
     ylab = "Post-census 5-year TFP growth (annualised log change)",
     pch = 19)
abline(lm(tfp_growth_post5 ~ rental_area_share, data = d_fig1), lwd = 2)
dev.off()

# Figure 2: adjusted relationship implied directly by approved main-model coefficients.
main_vars <- c("tfp_growth_post5", "rental_area_share", "log_average_holding_area",
               "log_gdp_per_capita", "agriculture_value_added_pct_gdp")
d_main <- d[complete.cases(d[, main_vars]), , drop = FALSE]
if (nrow(d_main) < 40) stop("Unexpectedly small complete main-model sample: ", nrow(d_main))
main_coef <- models[models$model == "main_post5", c("term", "estimate"), drop = FALSE]
required_coef <- c("(Intercept)", "rental_area_share", "log_average_holding_area",
                   "log_gdp_per_capita", "agriculture_value_added_pct_gdp")
if (!all(required_coef %in% main_coef$term)) stop("Missing approved main-model coefficients for Figure 2")
b <- setNames(as.numeric(main_coef$estimate), main_coef$term)
grid <- seq(min(d_main$rental_area_share), max(d_main$rental_area_share), length.out = 100)
fit <- b["(Intercept)"] +
  b["rental_area_share"] * grid +
  b["log_average_holding_area"] * mean(d_main$log_average_holding_area) +
  b["log_gdp_per_capita"] * mean(d_main$log_gdp_per_capita) +
  b["agriculture_value_added_pct_gdp"] * mean(d_main$agriculture_value_added_pct_gdp)
png(file.path(out_dir, "figure2_adjusted_rental_tfp.png"), width = 1800, height = 1200, res = 180)
plot(grid, fit, type = "l", lwd = 2,
     xlab = "Agricultural area rented from others (%)",
     ylab = "Adjusted post-census 5-year TFP growth")
dev.off()

# Generated Results narrative using approved estimates only.
# The journal-facing Results section is intentionally continuous prose: no internal
# subheadings are emitted here, so section hierarchy remains streamlined and reproducible.
main <- rental[rental$model == "main_post5" & rental$term == "rental_area_share", , drop = FALSE]
post3 <- rental[rental$model == "robust_post3" & rental$term == "rental_area_share", , drop = FALSE]
pre5 <- rental[rental$model == "diagnostic_pre5" & rental$term == "rental_area_share", , drop = FALSE]
noinf <- rental[rental$model == "robust_no_influential" & rental$term == "rental_area_share", , drop = FALSE]
inter <- rental[rental$model == "structural_interaction" & rental$term == "rental_c:small_c", , drop = FALSE]
adv <- function(name) advanced[advanced$model == name, , drop = FALSE]
fe <- adv("country_fe_repeated_censuses"); hold <- adv("alternative_rental_holding_share"); acc <- adv("tfp_growth_acceleration")
fe_n <- advanced_n[advanced_n$test == "country_fe_repeated_censuses", , drop = FALSE]

required_singletons <- list(main=main, post3=post3, pre5=pre5, noinf=noinf, inter=inter, fe=fe, hold=hold, acc=acc, fe_n=fe_n)
for (nm in names(required_singletons)) if (nrow(required_singletons[[nm]]) != 1) stop("Expected exactly one publication result for: ", nm)

results_text <- c(
  sprintf("The baseline cross-country analytical dataset contains %d countries, of which %d have complete covariates for the approved main specification. Table 1 summarises the distribution of post-census TFP growth, land-rental prevalence, farm structure, and development controls. The sample spans substantial variation in both rental intensity and farm structure. Figure 1 displays the unconditional cross-country relationship between rental prevalence and subsequent TFP growth, while Figure 2 presents the covariate-adjusted relationship implied by the approved linear specification.", nrow(d), nrow(d_main)),
  "",
  sprintf("In the main specification, a 10-percentage-point increase in the share of agricultural area rented from others is associated with a %.4f change in annualised post-census TFP growth (HC3 p = %s). The estimate is small relative to cross-country outcome variation and is not statistically distinguishable from zero at conventional levels. The farm-structure interaction is likewise imprecisely estimated (p = %s), providing no robust evidence that the rental-TFP association systematically varies with the national share of holdings below two hectares. Table 2 reports the underlying coefficients and HC3 inference.", as.numeric(main$estimate_per_10pp), fmtp(main$p_value), fmtp(inter$p_value)),
  "",
  sprintf("The predefined robustness exercises do not materially change that interpretation. Using three-year post-census TFP growth yields an estimated 10-percentage-point rental effect of %.4f (p = %s); using pre-census five-year TFP growth as a diagnostic yields %.4f (p = %s); and removing observations flagged by Cook's distance yields %.4f (p = %s). These checks differ in purpose—outcome horizon, temporal diagnostic, and influence sensitivity—yet none reveals a stable positive association masked by the baseline specification.", as.numeric(post3$estimate_per_10pp), fmtp(post3$p_value), as.numeric(pre5$estimate_per_10pp), fmtp(pre5$p_value), as.numeric(noinf$estimate_per_10pp), fmtp(noinf$p_value)),
  "",
  sprintf("Three additional checks reach the same qualitative conclusion. The alternative exposure based on the share of holdings rented from others produces an effect of %.4f per 10 percentage points (p = %s; N = %d). Defining the outcome as the acceleration of TFP growth after versus before the census produces %.4f (p = %s; N = %d). Finally, the repeated-census country fixed-effects specification uses %d observations from %d countries and produces an area-rental estimate of %.4f per 10 percentage points (p = %s). Because this within-country sample is small, the fixed-effects result is treated as exploratory robustness rather than as the principal estimate. Table 3 summarises these predefined checks.", as.numeric(hold$estimate_per_10pp), fmtp(hold$p_value), as.integer(hold$n), as.numeric(acc$estimate_per_10pp), fmtp(acc$p_value), as.integer(acc$n), as.integer(fe_n$n), as.integer(fe_n$countries), as.numeric(fe$estimate_per_10pp), fmtp(fe$p_value)),
  "",
  "Taken together, the estimates do not support a stable cross-country relationship between aggregate agricultural land-rental prevalence and subsequent national agricultural TFP growth in the available harmonised census sample. This is an associational result and should not be interpreted as evidence that rental markets have no productivity effects at the farm level or that rental-market reforms are ineffective."
)
writeLines(results_text, file.path(out_dir, "results_generated.md"), useBytes = TRUE)

cat("PUBLICATION_OUTPUTS_OK\n")
cat("Tables: 3 | Figures: 2 | Generated Results fragment: 1\n")
