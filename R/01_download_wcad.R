# 01_download_wcad.R -----------------------------------------------------
# Reproducible acquisition of FAOSTAT World Census of Agriculture (WCAD)
# structural data. No analytical transformations belong in this script.

options(timeout = max(600, getOption("timeout")))

raw_dir <- file.path("data_raw", "faostat_wcad")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# Official FAOSTAT bulk-download endpoint verified against the current
# datasets_E.json catalogue. Dataset code: WCAD.
source_url <- paste0(
  "https://bulks-faostat.fao.org/production/",
  "World_Census_Agriculture_E_All_Data_(Normalized).zip"
)
catalogue_url <- "https://bulks-faostat.fao.org/production/datasets_E.json"

zip_file <- file.path(raw_dir, "World_Census_Agriculture_E_All_Data_Normalized.zip")
unzip_dir <- file.path(raw_dir, "unzipped")
dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(zip_file) || is.na(file.info(zip_file)$size) || file.info(zip_file)$size == 0) {
  download.file(source_url, zip_file, mode = "wb", method = "libcurl", quiet = FALSE)
}

if (!file.exists(zip_file) || is.na(file.info(zip_file)$size) || file.info(zip_file)$size == 0) {
  stop("WCAD bulk ZIP could not be downloaded from the official FAOSTAT endpoint.")
}

unzipped_files <- unzip(zip_file, exdir = unzip_dir, overwrite = TRUE)
csv_files <- unzipped_files[grepl("\\.csv$", unzipped_files, ignore.case = TRUE)]

if (length(csv_files) == 0L) {
  stop("WCAD ZIP was downloaded but no CSV file was found inside it.")
}

# Prefer the normalized all-data CSV if multiple CSV files are present.
preferred <- csv_files[grepl("All_Data.*Normalized|Normalized.*All_Data", basename(csv_files), ignore.case = TRUE)]
selected_csv <- if (length(preferred) > 0L) preferred[[1]] else csv_files[[1]]

# Structural validation only: do not transform values here.
preview <- read.csv(selected_csv, nrows = 5, check.names = FALSE, stringsAsFactors = FALSE)
if (ncol(preview) < 5L) {
  stop("Downloaded WCAD CSV does not have a plausible normalized FAOSTAT structure.")
}

manifest <- data.frame(
  dataset_code = "WCAD",
  dataset_name = "World Census of Agriculture: Structural data from agricultural censuses",
  faostat_catalogue_url = catalogue_url,
  source_url = source_url,
  official_catalogue_update = "2026-04-27",
  downloaded_zip = zip_file,
  selected_csv = selected_csv,
  zip_bytes = unname(file.info(zip_file)$size),
  zip_md5 = unname(tools::md5sum(zip_file)),
  downloaded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
)

write.csv(
  manifest,
  file.path(raw_dir, "wcad_download_manifest.csv"),
  row.names = FALSE,
  na = ""
)

writeLines(names(preview), file.path(raw_dir, "wcad_columns.txt"))

cat("WCAD_DOWNLOAD_OK\n")
cat("Selected CSV:", selected_csv, "\n")
cat("ZIP bytes:", manifest$zip_bytes, "\n")
cat("Columns:", paste(names(preview), collapse = " | "), "\n")
