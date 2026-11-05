# 00_rebuild_analysis_from_source.R -----------------------------------------
# Reconstruct all non-versioned analytical inputs and approved outputs from
# official source data in a fresh clone.
#
# This script does not change specifications. It executes the already approved
# reproducible pipeline in its documented order.

stages <- c(
  "R/00_setup.R",
  "R/01_download_wcad.R",
  "R/02_audit_wcad.R",
  "R/02b_audit_wcad_comparability.R",
  "R/03_download_usda.R",
  "R/04_harmonize.R",
  "R/04b_audit_farm_structure.R",
  "R/04c_add_development_controls.R",
  "R/05_descriptive_diagnostics.R",
  "R/06_models_robustness.R",
  "R/07_advanced_robustness.R",
  "R/08_publication_outputs.R"
)

missing_scripts <- stages[!file.exists(stages)]
if (length(missing_scripts)) {
  stop("Pipeline script(s) missing: ", paste(missing_scripts, collapse = "; "))
}

cat("REBUILD_FROM_SOURCE_START\n")
for (stage in stages) {
  cat("\n>>> ", stage, "\n", sep = "")
  sys.source(stage, envir = .GlobalEnv)
}

required_outputs <- c(
  "outputs/audit/final_sample/analytical_sample_baseline.csv",
  "outputs/audit/final_sample/analytical_sample_structural.csv",
  "outputs/diagnostics/descriptive_statistics.csv",
  "outputs/models/model_results_hc3.csv",
  "outputs/models/rental_effect_summary.csv",
  "outputs/models/advanced/advanced_rental_effects.csv",
  "outputs/models/advanced/advanced_sample_summary.csv",
  "outputs/publication/results_generated.md",
  "outputs/publication/table1_descriptives.md",
  "outputs/publication/table2_main_models.md",
  "outputs/publication/table3_robustness.md",
  "outputs/publication/figure1_rental_tfp.png",
  "outputs/publication/figure2_adjusted_rental_tfp.png"
)

missing_outputs <- required_outputs[!file.exists(required_outputs)]
if (length(missing_outputs)) {
  stop("REBUILD_OUTPUT_AUDIT_FAIL: ", paste(missing_outputs, collapse = "; "))
}

baseline <- read.csv(
  "outputs/audit/final_sample/analytical_sample_baseline.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
main <- read.csv(
  "outputs/models/rental_effect_summary.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
main <- main[main$model == "main_post5" & main$term == "rental_area_share", , drop = FALSE]

if (nrow(baseline) != 65L) {
  stop("REPRODUCIBILITY_CHECK_FAIL: baseline N = ", nrow(baseline), "; expected approved N = 65.")
}
if (nrow(main) != 1L) {
  stop("REPRODUCIBILITY_CHECK_FAIL: approved main rental effect could not be uniquely identified.")
}

cat("\nREBUILD_FROM_SOURCE_OK\n")
cat("Baseline analytical sample N: ", nrow(baseline), "\n", sep = "")
cat("Main rental effect per 10 pp: ", format(main$estimate_per_10pp, digits = 8), "\n", sep = "")
cat("Main HC3 p-value: ", format(main$p_value, digits = 8), "\n", sep = "")
cat("All required publication outputs: PASS\n")
