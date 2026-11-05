# 02b_audit_wcad_comparability.R -----------------------------------------
# Temporal comparability audit for the preferred WCAD rental measure.
# Scientific scope: FAOSTAT WCAD only. No USDA data and no econometric models.

in_file <- file.path("outputs", "audit", "wcad", "rental_observations.csv")
out_dir <- file.path("outputs", "audit", "wcad_comparability")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(in_file)) {
  stop("WCAD rental observations are missing. Run R/01_download_wcad.R and R/02_audit_wcad.R first.")
}

rental <- read.csv(in_file, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))

required <- c("source_row", "country", "census_year", "unit", "measure_dimension", "value")
missing_cols <- setdiff(required, names(rental))
if (length(missing_cols) > 0L) {
  stop("Required columns missing from rental_observations.csv: ", paste(missing_cols, collapse = ", "))
}

country <- trimws(as.character(rental$country))
year <- suppressWarnings(as.integer(rental$census_year))
unit <- tolower(trimws(as.character(rental$unit)))
dimension <- tolower(trimws(as.character(rental$measure_dimension)))
value <- suppressWarnings(as.numeric(rental$value))

is_percent <- grepl("%|percent|percentage", unit)
is_area_percent <- dimension == "area" & is_percent
valid_key <- !is.na(country) & nzchar(country) & !is.na(year)
valid_value <- !is.na(value)

area_pct <- rental[is_area_percent & valid_key & valid_value, , drop = FALSE]
area_pct$country <- trimws(as.character(area_pct$country))
area_pct$census_year <- suppressWarnings(as.integer(area_pct$census_year))
area_pct$value <- suppressWarnings(as.numeric(area_pct$value))

if (nrow(area_pct) == 0L) stop("No valid rental-area percentage observations were found.")

# Percentage shares should be bounded. Values outside [0, 100] would indicate
# a unit/classification problem and must not silently enter the analytical pool.
out_of_bounds <- area_pct[area_pct$value < 0 | area_pct$value > 100, , drop = FALSE]
write.csv(out_of_bounds, file.path(out_dir, "out_of_bounds_area_percent.csv"), row.names = FALSE, na = "")
if (nrow(out_of_bounds) > 0L) {
  stop("Found ", nrow(out_of_bounds), " rental-area percentage observation(s) outside [0, 100].")
}

# Country-year uniqueness is audited explicitly. Multiple rows for the same
# country and census year are retained for inspection instead of arbitrarily
# choosing one observation.
key <- paste(area_pct$country, area_pct$census_year, sep = "\u001f")
key_counts <- table(key)
duplicate_keys <- names(key_counts[key_counts > 1L])

if (length(duplicate_keys) > 0L) {
  duplicate_rows <- area_pct[key %in% duplicate_keys, , drop = FALSE]
  duplicate_rows <- duplicate_rows[order(duplicate_rows$country, duplicate_rows$census_year, duplicate_rows$source_row), , drop = FALSE]
} else {
  duplicate_rows <- area_pct[0, , drop = FALSE]
}
write.csv(duplicate_rows, file.path(out_dir, "duplicate_country_year_area_percent.csv"), row.names = FALSE, na = "")

# For temporal coverage counts, a country-year is counted once. If duplicated
# rows exist, they do not inflate coverage; they remain flagged for later
# harmonisation before any analytical dataset is constructed.
area_pct_unique <- area_pct[!duplicated(key), , drop = FALSE]
area_pct_unique <- area_pct_unique[order(area_pct_unique$country, area_pct_unique$census_year), , drop = FALSE]
write.csv(area_pct_unique, file.path(out_dir, "rental_area_percent_country_year.csv"), row.names = FALSE, na = "")

countries <- sort(unique(area_pct_unique$country))
country_profile <- do.call(rbind, lapply(countries, function(ctry) {
  yrs <- sort(unique(area_pct_unique$census_year[area_pct_unique$country == ctry]))
  data.frame(
    country = ctry,
    n_area_percent_census_years = length(yrs),
    first_area_percent_census_year = min(yrs),
    latest_area_percent_census_year = max(yrs),
    span_years = max(yrs) - min(yrs),
    has_2plus_area_percent_censuses = length(yrs) >= 2L,
    has_3plus_area_percent_censuses = length(yrs) >= 3L,
    latest_since_2000 = max(yrs) >= 2000,
    latest_since_2010 = max(yrs) >= 2010,
    latest_since_2015 = max(yrs) >= 2015,
    latest_since_2020 = max(yrs) >= 2020,
    stringsAsFactors = FALSE
  )
}))
write.csv(country_profile, file.path(out_dir, "country_area_percent_temporal_profile.csv"), row.names = FALSE, na = "")

cutoffs <- c(1990L, 2000L, 2005L, 2010L, 2015L, 2020L)
cutoff_table <- do.call(rbind, lapply(cutoffs, function(cutoff) {
  obs <- area_pct_unique$census_year >= cutoff
  ctries <- unique(area_pct_unique$country[obs])
  repeated <- vapply(ctries, function(ctry) {
    length(unique(area_pct_unique$census_year[area_pct_unique$country == ctry & area_pct_unique$census_year >= cutoff])) >= 2L
  }, logical(1))
  data.frame(
    cutoff_year = cutoff,
    n_country_year_observations = sum(obs),
    n_countries = length(ctries),
    n_countries_with_2plus_censuses_since_cutoff = sum(repeated),
    stringsAsFactors = FALSE
  )
}))
write.csv(cutoff_table, file.path(out_dir, "temporal_cutoff_coverage.csv"), row.names = FALSE, na = "")

latest_year_distribution <- as.data.frame(table(country_profile$latest_area_percent_census_year), stringsAsFactors = FALSE)
names(latest_year_distribution) <- c("latest_area_percent_census_year", "n_countries")
write.csv(latest_year_distribution, file.path(out_dir, "latest_area_percent_year_distribution.csv"), row.names = FALSE, na = "")

summary <- data.frame(
  metric = c(
    "valid_rental_area_percent_observations",
    "unique_rental_area_percent_country_years",
    "countries_with_rental_area_percent",
    "duplicate_country_year_keys",
    "out_of_bounds_observations",
    "countries_with_2plus_area_percent_censuses",
    "countries_with_3plus_area_percent_censuses",
    "countries_latest_area_percent_since_2000",
    "countries_latest_area_percent_since_2010",
    "countries_latest_area_percent_since_2015",
    "countries_latest_area_percent_since_2020",
    "median_latest_area_percent_census_year"
  ),
  value = c(
    nrow(area_pct),
    nrow(area_pct_unique),
    nrow(country_profile),
    length(duplicate_keys),
    nrow(out_of_bounds),
    sum(country_profile$has_2plus_area_percent_censuses),
    sum(country_profile$has_3plus_area_percent_censuses),
    sum(country_profile$latest_since_2000),
    sum(country_profile$latest_since_2010),
    sum(country_profile$latest_since_2015),
    sum(country_profile$latest_since_2020),
    stats::median(country_profile$latest_area_percent_census_year)
  ),
  stringsAsFactors = FALSE
)
write.csv(summary, file.path(out_dir, "comparability_summary.csv"), row.names = FALSE, na = "")

lookup <- setNames(summary$value, summary$metric)
report <- c(
  "# WCAD rental-area temporal comparability audit",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "## Preferred measure",
  "",
  "This stage uses only observations already classified as percentage of agricultural area rented from others. It does not use the percentage of holdings rented.",
  "",
  "## Integrity checks",
  "",
  paste0("- Valid rental-area percentage observations: ", lookup[["valid_rental_area_percent_observations"]]),
  paste0("- Unique country-year observations: ", lookup[["unique_rental_area_percent_country_years"]]),
  paste0("- Country-year keys with duplicates: ", lookup[["duplicate_country_year_keys"]]),
  paste0("- Observations outside [0, 100]: ", lookup[["out_of_bounds_observations"]]),
  "",
  "## Temporal coverage",
  "",
  paste0("- Countries/territories with rental-area percentages: ", lookup[["countries_with_rental_area_percent"]]),
  paste0("- Countries with at least two area-percentage censuses: ", lookup[["countries_with_2plus_area_percent_censuses"]]),
  paste0("- Countries with at least three area-percentage censuses: ", lookup[["countries_with_3plus_area_percent_censuses"]]),
  paste0("- Countries whose latest observation is 2010 or later: ", lookup[["countries_latest_area_percent_since_2010"]]),
  paste0("- Countries whose latest observation is 2015 or later: ", lookup[["countries_latest_area_percent_since_2015"]]),
  paste0("- Countries whose latest observation is 2020 or later: ", lookup[["countries_latest_area_percent_since_2020"]]),
  paste0("- Median latest census year: ", lookup[["median_latest_area_percent_census_year"]]),
  "",
  "## Scientific boundary",
  "",
  "The cutoff table is descriptive evidence for choosing a defensible modern comparison window. No final analytical cutoff is selected here because the usable window must also be supported by the temporal coverage of the productivity source in the next stage.",
  "",
  "This audit does not merge USDA data, estimate productivity effects, or make causal claims."
)
writeLines(report, file.path(out_dir, "comparability_report.md"))

cat("WCAD_COMPARABILITY_AUDIT_OK\n")
print(summary, row.names = FALSE)
cat("\nTemporal cutoff coverage:\n")
print(cutoff_table, row.names = FALSE)
