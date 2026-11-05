# 05_descriptive_diagnostics.R ---------------------------------------------
# Descriptive statistics and minimal econometric diagnostics used only to
# choose a defensible functional form for the later main specification.
# This script does NOT define or report a final causal model.

in_file <- file.path("outputs", "audit", "final_sample", "analytical_sample_baseline.csv")
out_dir <- file.path("outputs", "diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(in_file)) stop("Final baseline analytical sample is missing. Run prior stages first.")
d <- read.csv(in_file, stringsAsFactors = FALSE, check.names = FALSE)

required <- c(
  "wcad_country", "census_year", "rental_area_share", "tfp_growth_post5",
  "average_holding_area_ha", "log_gdp_per_capita", "agriculture_value_added_pct_gdp"
)
missing_cols <- setdiff(required, names(d))
if (length(missing_cols) > 0L) stop("Baseline sample lacks: ", paste(missing_cols, collapse = ", "))

num_vars <- required[-c(1, 2)]
for (v in num_vars) d[[v]] <- suppressWarnings(as.numeric(d[[v]]))
d$census_year <- suppressWarnings(as.integer(d$census_year))

if (any(!complete.cases(d[, required]))) stop("Baseline diagnostic sample contains missing required values.")
if (any(d$average_holding_area_ha <= 0)) stop("Average holding area must be positive for log transformation.")
if (any(d$rental_area_share < 0 | d$rental_area_share > 100)) stop("Rental-area share falls outside [0,100].")

d$log_average_holding_area <- log(d$average_holding_area_ha)
d$rental_centered <- d$rental_area_share - mean(d$rental_area_share)
d$rental_centered_sq <- d$rental_centered^2

# ---- Descriptive statistics ---------------------------------------------
describe_one <- function(x, variable) {
  q <- stats::quantile(x, probs = c(0, .05, .25, .5, .75, .95, 1), na.rm = TRUE, names = FALSE)
  data.frame(
    variable = variable,
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = stats::sd(x, na.rm = TRUE),
    min = q[[1]], p05 = q[[2]], p25 = q[[3]], median = q[[4]],
    p75 = q[[5]], p95 = q[[6]], max = q[[7]],
    stringsAsFactors = FALSE
  )
}

descriptive_vars <- c(
  "tfp_growth_post5", "rental_area_share", "average_holding_area_ha",
  "log_average_holding_area", "log_gdp_per_capita", "agriculture_value_added_pct_gdp"
)
descriptives <- do.call(rbind, lapply(descriptive_vars, function(v) describe_one(d[[v]], v)))
write.csv(descriptives, file.path(out_dir, "descriptive_statistics.csv"), row.names = FALSE, na = "")

# Pairwise correlations are descriptive only, not a selection criterion.
cor_matrix <- stats::cor(d[, descriptive_vars], use = "complete.obs")
write.csv(cor_matrix, file.path(out_dir, "correlation_matrix.csv"), row.names = TRUE, na = "")

# ---- Robust univariate outlier flags (1.5 IQR) ---------------------------
iqr_flag <- function(x) {
  q <- stats::quantile(x, c(.25, .75), na.rm = TRUE, names = FALSE)
  iqr <- q[[2]] - q[[1]]
  x < (q[[1]] - 1.5 * iqr) | x > (q[[2]] + 1.5 * iqr)
}

flag_vars <- c("tfp_growth_post5", "rental_area_share", "average_holding_area_ha")
outlier_rows <- data.frame(
  wcad_country = d$wcad_country,
  census_year = d$census_year,
  tfp_growth_post5 = d$tfp_growth_post5,
  rental_area_share = d$rental_area_share,
  average_holding_area_ha = d$average_holding_area_ha,
  stringsAsFactors = FALSE
)
for (v in flag_vars) outlier_rows[[paste0(v, "_iqr_flag")]] <- iqr_flag(d[[v]])
outlier_rows$any_iqr_flag <- Reduce(`|`, outlier_rows[paste0(flag_vars, "_iqr_flag")])
write.csv(outlier_rows, file.path(out_dir, "univariate_outlier_flags.csv"), row.names = FALSE, na = "")

# ---- Minimal functional-form diagnostic ---------------------------------
# Average holding area is logged ex ante because its cross-country scale is
# intrinsically multiplicative and strongly right-skewed in agricultural data.
# Rental share is tested as linear versus a centered quadratic extension.

m_linear <- stats::lm(
  tfp_growth_post5 ~ rental_centered + log_average_holding_area +
    log_gdp_per_capita + agriculture_value_added_pct_gdp,
  data = d
)

m_quadratic <- stats::lm(
  tfp_growth_post5 ~ rental_centered + rental_centered_sq +
    log_average_holding_area + log_gdp_per_capita + agriculture_value_added_pct_gdp,
  data = d
)

quad_test <- stats::anova(m_linear, m_quadratic)
quadratic_p <- quad_test$`Pr(>F)`[[2]]

# Ramsey-style RESET diagnostic: add fitted^2 and fitted^3 to the linear
# specification. This tests broad functional-form misspecification without
# turning the diagnostic stage into model fishing.
d$fit_linear <- stats::fitted(m_linear)
d$fit_linear_sq <- d$fit_linear^2
d$fit_linear_cu <- d$fit_linear^3
m_reset <- stats::lm(
  tfp_growth_post5 ~ rental_centered + log_average_holding_area +
    log_gdp_per_capita + agriculture_value_added_pct_gdp +
    fit_linear_sq + fit_linear_cu,
  data = d
)
reset_test <- stats::anova(m_linear, m_reset)
reset_p <- reset_test$`Pr(>F)`[[2]]

# Breusch-Pagan LM test implemented with base R: regress squared residuals on
# the original linear regressors (excluding intercept); LM = n * R^2.
e2 <- stats::residuals(m_linear)^2
X <- stats::model.matrix(m_linear)[, -1, drop = FALSE]
bp_aux <- stats::lm(e2 ~ X)
bp_lm <- nrow(d) * summary(bp_aux)$r.squared
bp_df <- ncol(X)
bp_p <- stats::pchisq(bp_lm, df = bp_df, lower.tail = FALSE)

# Influence diagnostics are flags for robustness analysis only. Observations
# are never deleted automatically.
cooks <- stats::cooks.distance(m_linear)
rstudent_vals <- stats::rstudent(m_linear)
influence <- data.frame(
  wcad_country = d$wcad_country,
  census_year = d$census_year,
  cooks_distance = cooks,
  cooks_flag_4_over_n = cooks > (4 / nrow(d)),
  studentized_residual = rstudent_vals,
  studentized_abs_gt2 = abs(rstudent_vals) > 2,
  stringsAsFactors = FALSE
)
write.csv(influence, file.path(out_dir, "influence_diagnostics.csv"), row.names = FALSE, na = "")

model_metrics <- data.frame(
  model = c("linear", "quadratic_rental"),
  n = c(stats::nobs(m_linear), stats::nobs(m_quadratic)),
  r_squared = c(summary(m_linear)$r.squared, summary(m_quadratic)$r.squared),
  adjusted_r_squared = c(summary(m_linear)$adj.r.squared, summary(m_quadratic)$adj.r.squared),
  aic = c(stats::AIC(m_linear), stats::AIC(m_quadratic)),
  bic = c(stats::BIC(m_linear), stats::BIC(m_quadratic)),
  stringsAsFactors = FALSE
)
write.csv(model_metrics, file.path(out_dir, "diagnostic_model_metrics.csv"), row.names = FALSE, na = "")

diagnostics <- data.frame(
  diagnostic = c(
    "quadratic_rental_added_term_F_test_p",
    "reset_F_test_p",
    "breusch_pagan_LM",
    "breusch_pagan_df",
    "breusch_pagan_p",
    "n_iqr_flagged_any_primary_variable",
    "n_cooks_distance_gt_4_over_n",
    "n_abs_studentized_residual_gt_2"
  ),
  value = c(
    quadratic_p,
    reset_p,
    bp_lm,
    bp_df,
    bp_p,
    sum(outlier_rows$any_iqr_flag),
    sum(influence$cooks_flag_4_over_n),
    sum(influence$studentized_abs_gt2)
  ),
  stringsAsFactors = FALSE
)
write.csv(diagnostics, file.path(out_dir, "diagnostic_summary.csv"), row.names = FALSE, na = "")

cat("DESCRIPTIVE_DIAGNOSTICS_OK\n")
cat("N =", nrow(d), "\n\n")
cat("Descriptive statistics:\n")
print(descriptives, row.names = FALSE)
cat("\nModel diagnostics:\n")
print(diagnostics, row.names = FALSE)
cat("\nDiagnostic model metrics:\n")
print(model_metrics, row.names = FALSE)
cat("\nNo observations were deleted and no final model was selected by this script.\n")
