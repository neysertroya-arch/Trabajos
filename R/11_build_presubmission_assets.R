# 11_build_presubmission_assets.R ------------------------------------------
# Build journal-facing presubmission assets from already approved analytical
# outputs. No inferential model is re-estimated and no reported coefficient is
# changed. The PDFs reproduce the two approved publication figures.

out_dir <- file.path("outputs", "submission", "land-use-policy")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sample_file <- file.path("outputs", "audit", "final_sample", "analytical_sample_baseline.csv")
model_file <- file.path("outputs", "models", "model_results_hc3.csv")
for (f in c(sample_file, model_file)) if (!file.exists(f)) stop("Missing approved analytical output: ", f)

d <- read.csv(sample_file, stringsAsFactors = FALSE, check.names = FALSE)
models <- read.csv(model_file, stringsAsFactors = FALSE, check.names = FALSE)

num_vars <- c("rental_area_share", "tfp_growth_post5", "average_holding_area_ha",
              "log_gdp_per_capita", "agriculture_value_added_pct_gdp")
for (v in num_vars) d[[v]] <- suppressWarnings(as.numeric(d[[v]]))
d$log_average_holding_area <- log(d$average_holding_area_ha)

# Figure 1: vector version of the approved descriptive plot.
d_fig1 <- d[complete.cases(d[, c("rental_area_share", "tfp_growth_post5")]), , drop = FALSE]
pdf(file.path(out_dir, "Figure_1.pdf"), width = 10, height = 6.67, useDingbats = FALSE)
plot(d_fig1$rental_area_share, d_fig1$tfp_growth_post5,
     xlab = "Agricultural area rented from others (%)",
     ylab = "Post-census 5-year TFP growth (annualized log change)",
     pch = 19)
abline(lm(tfp_growth_post5 ~ rental_area_share, data = d_fig1), lwd = 2)
dev.off()

# Figure 2: vector version implied by approved main-model coefficients.
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
pdf(file.path(out_dir, "Figure_2.pdf"), width = 10, height = 6.67, useDingbats = FALSE)
plot(grid, fit, type = "l", lwd = 2,
     xlab = "Agricultural area rented from others (%)",
     ylab = "Adjusted post-census 5-year TFP growth")
dev.off()

captions <- c(
  "# Figure captions",
  "",
  "**Figure 1.** Unconditional relationship between agricultural land-rental prevalence and subsequent TFP growth. The fitted line is descriptive and does not represent a causal effect.",
  "",
  "**Figure 2.** Covariate-adjusted relationship implied directly by the approved baseline linear specification, with other covariates held at their sample means."
)
writeLines(captions, file.path(out_dir, "figure-captions.md"), useBytes = TRUE)

for (f in c("Figure_1.pdf", "Figure_2.pdf", "figure-captions.md")) {
  if (!file.exists(file.path(out_dir, f))) stop("Presubmission asset not created: ", f)
}

cat("LUP_PRESUBMISSION_ASSETS_OK\n")
cat("Vector figures: 2 | Caption source: 1\n")
