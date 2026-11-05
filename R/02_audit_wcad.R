# 02_audit_wcad.R --------------------------------------------------------
# Feasibility audit for FAOSTAT WCAD structural census data.
# This script describes coverage and comparability only; it estimates no model.

out_dir <- file.path("outputs", "audit", "wcad")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

manifest_file <- file.path("data_raw", "faostat_wcad", "wcad_download_manifest.csv")
if (!file.exists(manifest_file)) {
  stop("WCAD download manifest missing. Run R/01_download_wcad.R first.")
}

manifest <- read.csv(manifest_file, stringsAsFactors = FALSE, check.names = FALSE)
wcad_file <- manifest$selected_csv[[1]]
if (!file.exists(wcad_file)) stop("WCAD CSV referenced in the manifest does not exist.")

wcad <- read.csv(
  wcad_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

clean_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "_", x)
}

find_col <- function(candidates, required = TRUE) {
  nms <- clean_name(names(wcad))
  idx <- match(candidates, nms, nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx) > 0L) return(names(wcad)[idx[[1]]])
  if (required) stop("Could not detect required column: ", paste(candidates, collapse = ", "))
  NA_character_
}

area_col <- find_col(c("area", "country", "area_name", "country_name"))
item_col <- find_col(c("item", "item_name", "indicator", "indicator_name"))
year_col <- find_col(c("year", "census_year", "year_code"))
unit_col <- find_col(c("unit"), required = FALSE)
value_col <- find_col(c("value", "obs_value", "observation_value"))
flag_col <- find_col(c("flag", "flag_description"), required = FALSE)
note_col <- find_col(c("note", "notes"), required = FALSE)

source_row <- seq_len(nrow(wcad))
area <- trimws(as.character(wcad[[area_col]]))
item <- trimws(as.character(wcad[[item_col]]))
year <- suppressWarnings(as.integer(wcad[[year_col]]))
value <- suppressWarnings(as.numeric(wcad[[value_col]]))
unit <- if (!is.na(unit_col)) trimws(as.character(wcad[[unit_col]])) else rep(NA_character_, nrow(wcad))

valid_area <- !is.na(area) & nzchar(area)
valid_year <- !is.na(year)
valid_value <- !is.na(value)

item_lower <- tolower(item)
unit_lower <- tolower(unit)

# Broad tenure catalogue and specific rental measures. WCAD uses the same Item
# label (e.g. "Holdings rented from others") for area and holding counts, so
# the measurement dimension cannot be inferred from the Item text alone.
is_tenure <- grepl("tenure|rent|owned|owner", item_lower)
is_rental <- grepl("rent", item_lower)
is_percent <- grepl("%|percent|percentage", unit_lower)
is_hectare <- grepl("^ha$|hectare", unit_lower)
is_number <- grepl("^no$|number", unit_lower)

# In the normalized WCAD source, percentage observations are paired in source
# order with the preceding absolute observation within the same Area-Year-Item
# block. We therefore inherit the measurement dimension from that absolute
# observation: ha -> area; No -> holdings. The original source row is retained
# in the audit extract so this pairing remains directly inspectable.
rental_dimension <- rep(NA_character_, nrow(wcad))
rental_idx <- which(is_rental & valid_area & valid_year)

if (length(rental_idx) > 0L) {
  group_key <- paste(
    area[rental_idx],
    year[rental_idx],
    item[rental_idx],
    sep = "\u001f"
  )

  for (idx in split(rental_idx, group_key)) {
    last_dimension <- NA_character_

    for (i in idx) {
      if (is_hectare[i]) {
        last_dimension <- "area"
        rental_dimension[i] <- "area"
      } else if (is_number[i]) {
        last_dimension <- "holdings"
        rental_dimension[i] <- "holdings"
      } else if (is_percent[i] && !is.na(last_dimension)) {
        rental_dimension[i] <- last_dimension
      }
    }
  }
}

unclassified_rental_percent <-
  is_rental & is_percent & valid_area & valid_year & valid_value &
  is.na(rental_dimension)

if (any(unclassified_rental_percent, na.rm = TRUE)) {
  stop(
    "WCAD rental percentage classification is ambiguous: ",
    sum(unclassified_rental_percent, na.rm = TRUE),
    " valid percentage observation(s) could not be assigned to area or holdings."
  )
}

is_rental_area <- is_rental & !is.na(rental_dimension) & rental_dimension == "area"
is_rental_holding <- is_rental & !is.na(rental_dimension) & rental_dimension == "holdings"

area_pct_countries <- unique(
  area[is_rental_area & is_percent & valid_area & valid_value]
)
holding_pct_countries <- unique(
  area[is_rental_holding & is_percent & valid_area & valid_value]
)
both_pct_countries <- intersect(area_pct_countries, holding_pct_countries)

profile <- data.frame(
  metric = c(
    "rows_total",
    "countries_or_territories_total",
    "min_census_year_total",
    "max_census_year_total",
    "tenure_rows",
    "countries_with_any_tenure_information",
    "rental_rows",
    "countries_with_any_rental_information",
    "countries_with_rental_area_measure",
    "countries_with_rental_holding_measure",
    "countries_with_rental_area_percent",
    "countries_with_rental_holding_percent",
    "countries_with_both_rental_percent_measures",
    "rental_area_percent_census_observations",
    "rental_holding_percent_census_observations",
    "unclassified_rental_percent_observations",
    "countries_with_2plus_rental_census_years",
    "countries_with_rental_record_since_2000",
    "countries_with_rental_record_since_2010",
    "countries_with_rental_record_since_2015",
    "countries_with_rental_record_since_2020"
  ),
  value = c(
    nrow(wcad),
    length(unique(area[valid_area])),
    if (any(valid_year)) min(year[valid_year]) else NA,
    if (any(valid_year)) max(year[valid_year]) else NA,
    sum(is_tenure, na.rm = TRUE),
    length(unique(area[is_tenure & valid_area])),
    sum(is_rental, na.rm = TRUE),
    length(unique(area[is_rental & valid_area])),
    length(unique(area[is_rental_area & valid_area])),
    length(unique(area[is_rental_holding & valid_area])),
    length(area_pct_countries),
    length(holding_pct_countries),
    length(both_pct_countries),
    sum(is_rental_area & is_percent & valid_area & valid_year & valid_value, na.rm = TRUE),
    sum(is_rental_holding & is_percent & valid_area & valid_year & valid_value, na.rm = TRUE),
    sum(unclassified_rental_percent, na.rm = TRUE),
    {
      tmp <- split(year[is_rental & valid_area & valid_year & valid_value], area[is_rental & valid_area & valid_year & valid_value])
      sum(vapply(tmp, function(x) length(unique(x)) >= 2L, logical(1)))
    },
    length(unique(area[is_rental & valid_area & valid_year & year >= 2000])),
    length(unique(area[is_rental & valid_area & valid_year & year >= 2010])),
    length(unique(area[is_rental & valid_area & valid_year & year >= 2015])),
    length(unique(area[is_rental & valid_area & valid_year & year >= 2020]))
  ),
  stringsAsFactors = FALSE
)

write.csv(profile, file.path(out_dir, "audit_summary.csv"), row.names = FALSE, na = "")

# Catalogue the exact tenure/rental indicator labels and units.
catalogue_rows <- is_tenure & !is.na(item) & nzchar(item)
catalogue <- unique(data.frame(
  item = item[catalogue_rows],
  unit = unit[catalogue_rows],
  stringsAsFactors = FALSE
))
catalogue <- catalogue[order(tolower(catalogue$item), tolower(catalogue$unit)), , drop = FALSE]
write.csv(catalogue, file.path(out_dir, "tenure_indicator_catalogue.csv"), row.names = FALSE, na = "")

rental_rows <- is_rental & valid_area & valid_year
rental_extract <- data.frame(
  source_row = source_row[rental_rows],
  country = area[rental_rows],
  census_year = year[rental_rows],
  item = item[rental_rows],
  unit = unit[rental_rows],
  measure_dimension = rental_dimension[rental_rows],
  value = value[rental_rows],
  stringsAsFactors = FALSE
)
if (!is.na(flag_col)) rental_extract$flag <- wcad[[flag_col]][rental_rows]
if (!is.na(note_col)) rental_extract$note <- wcad[[note_col]][rental_rows]
rental_extract <- rental_extract[
  order(
    rental_extract$country,
    rental_extract$census_year,
    rental_extract$item,
    rental_extract$source_row
  ),
  ,
  drop = FALSE
]
write.csv(rental_extract, file.path(out_dir, "rental_observations.csv"), row.names = FALSE, na = "")

# Country-level coverage: latest year, dimension-specific percentage coverage,
# and repeated-census potential.
rental_valid <- is_rental & valid_area & valid_year & valid_value
if (any(rental_valid)) {
  countries <- sort(unique(area[rental_valid]))
  coverage <- do.call(rbind, lapply(countries, function(ctry) {
    idx <- rental_valid & area == ctry
    yrs <- sort(unique(year[idx]))
    has_area_pct <- any(idx & is_rental_area & is_percent & valid_value)
    has_holding_pct <- any(idx & is_rental_holding & is_percent & valid_value)

    data.frame(
      country = ctry,
      n_rental_census_years = length(yrs),
      first_rental_census_year = min(yrs),
      latest_rental_census_year = max(yrs),
      has_rental_area_percent = has_area_pct,
      has_rental_holding_percent = has_holding_pct,
      has_both_rental_percent_measures = has_area_pct & has_holding_pct,
      has_repeated_census = length(yrs) >= 2L,
      stringsAsFactors = FALSE
    )
  }))
} else {
  coverage <- data.frame()
}
write.csv(coverage, file.path(out_dir, "country_rental_coverage.csv"), row.names = FALSE, na = "")

# Latest-rental-year distribution helps judge how much of the cross-section can
# be aligned with modern productivity data.
if (nrow(coverage) > 0L) {
  latest_distribution <- as.data.frame(table(coverage$latest_rental_census_year), stringsAsFactors = FALSE)
  names(latest_distribution) <- c("latest_rental_census_year", "n_countries")
} else {
  latest_distribution <- data.frame(latest_rental_census_year = integer(), n_countries = integer())
}
write.csv(latest_distribution, file.path(out_dir, "latest_rental_year_distribution.csv"), row.names = FALSE, na = "")

# Human-readable audit report generated from the same computed objects.
lookup <- setNames(profile$value, profile$metric)
report <- c(
  "# WCAD feasibility audit",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "## Core coverage",
  "",
  paste0("- Total rows: ", lookup[["rows_total"]]),
  paste0("- Countries/territories in WCAD: ", lookup[["countries_or_territories_total"]]),
  paste0("- Countries/territories with any tenure information: ", lookup[["countries_with_any_tenure_information"]]),
  paste0("- Countries/territories with any rental information: ", lookup[["countries_with_any_rental_information"]]),
  paste0("- Countries/territories with rental-area percentage observations: ", lookup[["countries_with_rental_area_percent"]]),
  paste0("- Countries/territories with rental-holding percentage observations: ", lookup[["countries_with_rental_holding_percent"]]),
  paste0("- Countries/territories with both rental percentage measures: ", lookup[["countries_with_both_rental_percent_measures"]]),
  paste0("- Unclassified valid rental percentage observations: ", lookup[["unclassified_rental_percent_observations"]]),
  paste0("- Countries with two or more rental census years: ", lookup[["countries_with_2plus_rental_census_years"]]),
  "",
  "## Measurement classification",
  "",
  "WCAD disseminates area and holding-count observations under the same rental Item label. Percentage rows are therefore classified by the measurement dimension of the preceding absolute-unit row within the same Area-Year-Item source block (ha = area; No = holdings). The original source row is retained in rental_observations.csv for direct auditability, and the script stops if any valid rental percentage cannot be classified.",
  "",
  "## Methodological caution",
  "",
  "FAO explicitly notes that countries may apply their own definitions of agricultural holdings, which can affect international comparability. Therefore this audit establishes data feasibility, not causal identification or full harmonization.",
  "",
  "The next scientific stage should inspect the corrected coverage and then test overlap with the productivity source before any econometric design is fixed."
)
writeLines(report, file.path(out_dir, "audit_report.md"))

cat("WCAD_FEASIBILITY_AUDIT_OK\n")
print(profile, row.names = FALSE)
