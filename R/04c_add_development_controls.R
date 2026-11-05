# 04c_add_development_controls.R
# Add parsimonious pre-/contemporaneous development controls to the candidate sample.
# Scientific boundary: data construction and coverage audit only; no regressions.

options(timeout = max(1200, getOption("timeout")))

in_file <- file.path("outputs", "audit", "farm_structure", "candidate_sample_with_structure.csv")
usda_manifest_file <- file.path("data_raw", "usda_tfp", "usda_tfp_download_manifest.csv")
raw_dir <- file.path("data_raw", "wdi")
out_dir <- file.path("outputs", "audit", "final_sample")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (f in c(in_file, usda_manifest_file)) {
  if (!file.exists(f)) stop("Required input missing: ", f)
}

sample <- read.csv(in_file, stringsAsFactors = FALSE, check.names = FALSE)
required_sample <- c("fao_code", "wcad_country", "census_year", "rental_area_share", "tfp_growth_post5", "average_holding_area_ha", "small_holdings_share_lt2")
missing_sample <- setdiff(required_sample, names(sample))
if (length(missing_sample) > 0L) stop("Candidate sample lacks required columns: ", paste(missing_sample, collapse = ", "))

sample$fao_code <- suppressWarnings(as.integer(sample$fao_code))
sample$census_year <- suppressWarnings(as.integer(sample$census_year))

# Recover ISO3 from the already validated USDA source using the stable FAO code.
# This avoids introducing a second country-name matching procedure.
usda_manifest <- read.csv(usda_manifest_file, stringsAsFactors = FALSE, check.names = FALSE)
usda <- read.csv(usda_manifest$downloaded_file[[1]], stringsAsFactors = FALSE, check.names = FALSE)
if (!all(c("FAO", "ISO3") %in% names(usda))) stop("USDA file lacks FAO and ISO3 columns.")
usda_map <- unique(data.frame(
  fao_code = suppressWarnings(as.integer(usda$FAO)),
  iso3 = trimws(as.character(usda$ISO3)),
  stringsAsFactors = FALSE
))
usda_map <- usda_map[!is.na(usda_map$fao_code) & !is.na(usda_map$iso3) & nzchar(usda_map$iso3), , drop = FALSE]
conflicting_iso3 <- vapply(split(usda_map$iso3, usda_map$fao_code), function(x) length(unique(x)) > 1L, logical(1))
if (any(conflicting_iso3)) stop("USDA contains conflicting ISO3 values for a FAO code.")
usda_map <- usda_map[!duplicated(usda_map$fao_code), , drop = FALSE]
sample$iso3 <- usda_map$iso3[match(sample$fao_code, usda_map$fao_code)]
if (any(is.na(sample$iso3) | !nzchar(sample$iso3))) stop("ISO3 could not be recovered for all candidate countries from USDA FAO codes.")

indicators <- data.frame(
  code = c("NY.GDP.PCAP.KD", "NV.AGR.TOTL.ZS"),
  variable = c("gdp_per_capita_constant_2015_usd", "agriculture_value_added_pct_gdp"),
  stringsAsFactors = FALSE
)

read_wdi_indicator <- function(code, variable) {
  url <- paste0("https://api.worldbank.org/v2/country/all/indicator/", code, "?source=2&downloadformat=csv")
  zip_file <- file.path(raw_dir, paste0(code, ".zip"))
  unzip_dir <- file.path(raw_dir, gsub("[^A-Za-z0-9]+", "_", code))
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

  download.file(url, zip_file, mode = "wb", method = "libcurl", quiet = FALSE)
  if (!file.exists(zip_file) || is.na(file.info(zip_file)$size) || file.info(zip_file)$size == 0) {
    stop("WDI download failed for ", code)
  }

  files <- unzip(zip_file, exdir = unzip_dir, overwrite = TRUE)
  csv <- files[grepl("API_.*\\.csv$", basename(files), ignore.case = TRUE)]
  csv <- csv[!grepl("Metadata", basename(csv), ignore.case = TRUE)]
  if (length(csv) != 1L) stop("Could not uniquely identify WDI data CSV for ", code)

  d <- read.csv(csv[[1]], skip = 4, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("Country Code", "Indicator Code")
  if (!all(required %in% names(d))) stop("Unexpected WDI schema for ", code)
  d <- d[d[["Indicator Code"]] == code, , drop = FALSE]

  year_cols <- grep("^[0-9]{4}$", names(d), value = TRUE)
  if (length(year_cols) == 0L) stop("No year columns found for WDI indicator ", code)

  long <- do.call(rbind, lapply(year_cols, function(y) {
    data.frame(
      iso3 = trimws(as.character(d[["Country Code"]])),
      year = as.integer(y),
      value = suppressWarnings(as.numeric(d[[y]])),
      stringsAsFactors = FALSE
    )
  }))
  long <- long[!is.na(long$value) & nzchar(long$iso3), , drop = FALSE]
  names(long)[names(long) == "value"] <- variable

  manifest <- data.frame(
    indicator_code = code,
    variable = variable,
    source_url = url,
    downloaded_zip = zip_file,
    zip_bytes = unname(file.info(zip_file)$size),
    zip_md5 = unname(tools::md5sum(zip_file)),
    downloaded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  )
  write.csv(manifest, file.path(raw_dir, paste0(code, "_manifest.csv")), row.names = FALSE, na = "")
  long
}

wdi_list <- Map(read_wdi_indicator, indicators$code, indicators$variable)
names(wdi_list) <- indicators$variable

lookup_prior <- function(iso3, census_year, d, value_col, max_lag = 2L) {
  z <- d[d$iso3 == iso3 & d$year <= census_year & d$year >= census_year - max_lag, , drop = FALSE]
  if (nrow(z) == 0L) return(c(value = NA_real_, year = NA_real_))
  z <- z[order(-z$year), , drop = FALSE]
  c(value = z[[value_col]][[1]], year = z$year[[1]])
}

for (variable in indicators$variable) {
  d <- wdi_list[[variable]]
  ans <- t(mapply(
    lookup_prior,
    sample$iso3,
    sample$census_year,
    MoreArgs = list(d = d, value_col = variable, max_lag = 2L)
  ))
  sample[[variable]] <- as.numeric(ans[, "value"])
  sample[[paste0(variable, "_year")]] <- as.integer(ans[, "year"])
  sample[[paste0(variable, "_lag_years")]] <- sample$census_year - sample[[paste0(variable, "_year")]]
}

sample$log_gdp_per_capita <- ifelse(
  !is.na(sample$gdp_per_capita_constant_2015_usd) & sample$gdp_per_capita_constant_2015_usd > 0,
  log(sample$gdp_per_capita_constant_2015_usd),
  NA_real_
)

sample$baseline_complete <- with(sample,
  !is.na(rental_area_share) & !is.na(tfp_growth_post5) & !is.na(average_holding_area_ha)
)
sample$development_complete <- with(sample,
  baseline_complete & !is.na(log_gdp_per_capita) & !is.na(agriculture_value_added_pct_gdp)
)
sample$structural_complete <- with(sample,
  development_complete & !is.na(small_holdings_share_lt2)
)

coverage <- data.frame(
  sample_definition = c(
    "candidate_post5_since2000",
    "baseline_rental_tfp_avg_size",
    "baseline_plus_development_controls",
    "full_plus_small_holdings_share"
  ),
  n_countries = c(
    nrow(sample),
    sum(sample$baseline_complete),
    sum(sample$development_complete),
    sum(sample$structural_complete)
  ),
  stringsAsFactors = FALSE
)

control_coverage <- data.frame(
  variable = c(
    "gdp_per_capita_constant_2015_usd",
    "agriculture_value_added_pct_gdp",
    "both_development_controls"
  ),
  n_countries = c(
    sum(!is.na(sample$gdp_per_capita_constant_2015_usd)),
    sum(!is.na(sample$agriculture_value_added_pct_gdp)),
    sum(!is.na(sample$gdp_per_capita_constant_2015_usd) & !is.na(sample$agriculture_value_added_pct_gdp))
  ),
  stringsAsFactors = FALSE
)

if (any(!is.na(sample$agriculture_value_added_pct_gdp) & (sample$agriculture_value_added_pct_gdp < 0 | sample$agriculture_value_added_pct_gdp > 100))) {
  stop("Agriculture value-added share contains observations outside [0,100].")
}
if (any(!is.na(sample$gdp_per_capita_constant_2015_usd) & sample$gdp_per_capita_constant_2015_usd <= 0)) {
  stop("GDP per capita contains non-positive observations.")
}

write.csv(sample, file.path(out_dir, "analytical_sample_audit.csv"), row.names = FALSE, na = "")
write.csv(sample[sample$development_complete, , drop = FALSE], file.path(out_dir, "analytical_sample_baseline.csv"), row.names = FALSE, na = "")
write.csv(sample[sample$structural_complete, , drop = FALSE], file.path(out_dir, "analytical_sample_structural.csv"), row.names = FALSE, na = "")
write.csv(coverage, file.path(out_dir, "final_sample_coverage.csv"), row.names = FALSE, na = "")
write.csv(control_coverage, file.path(out_dir, "development_control_coverage.csv"), row.names = FALSE, na = "")

cat("FINAL_SAMPLE_AUDIT_OK\n")
cat("\nFinal sample coverage:\n")
print(coverage, row.names = FALSE)
cat("\nDevelopment-control coverage:\n")
print(control_coverage, row.names = FALSE)
