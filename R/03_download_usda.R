# 03_download_usda.R -----------------------------------------------------
# Reproducible download of USDA ERS International Agricultural Productivity
# machine-readable long-format data (annual series, 1961-2023).
# No analytical transformations belong in this script.

options(timeout = max(1200, getOption("timeout")))

raw_dir <- file.path("data_raw", "usda_tfp")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

product_url <- "https://www.ers.usda.gov/data-products/international-agricultural-productivity"
source_url <- paste0(
  "https://www.ers.usda.gov/media/5403/",
  "machine-readable-and-long-format-file-of-tfp-indices-and-components-for-",
  "countries-regions-countries-grouped-by-income-level-and-the-world-1961-2023.csv?v=90858"
)

csv_file <- file.path(raw_dir, "AgTFPInternational2023_long.csv")

if (!file.exists(csv_file) || is.na(file.info(csv_file)$size) || file.info(csv_file)$size == 0) {
  download.file(
    source_url,
    destfile = csv_file,
    mode = "wb",
    method = "libcurl",
    quiet = FALSE
  )
}

if (!file.exists(csv_file) || is.na(file.info(csv_file)$size) || file.info(csv_file)$size == 0) {
  stop("USDA ERS International Agricultural Productivity CSV could not be downloaded.")
}

preview <- read.csv(
  csv_file,
  nrows = 10,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (ncol(preview) < 4L) {
  stop("Downloaded USDA ERS file does not have a plausible long-format tabular structure.")
}

manifest <- data.frame(
  dataset = "USDA ERS International Agricultural Productivity",
  release_date = "2026-01-27",
  temporal_coverage = "1961-2023",
  tfp_base_year = "2015=100",
  product_url = product_url,
  source_url = source_url,
  downloaded_file = csv_file,
  file_bytes = unname(file.info(csv_file)$size),
  file_md5 = unname(tools::md5sum(csv_file)),
  downloaded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
)

write.csv(
  manifest,
  file.path(raw_dir, "usda_tfp_download_manifest.csv"),
  row.names = FALSE,
  na = ""
)

writeLines(names(preview), file.path(raw_dir, "usda_tfp_columns.txt"))

cat("USDA_TFP_DOWNLOAD_OK\n")
cat("File bytes:", manifest$file_bytes, "\n")
cat("Columns:", paste(names(preview), collapse = " | "), "\n")
