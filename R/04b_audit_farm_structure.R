# Audit WCAD farm-structure measures in the candidate WCAD-USDA sample.
# Scientific boundary: coverage/measurement audit only; no regressions.

wcad_manifest_file <- file.path("data_raw", "faostat_wcad", "wcad_download_manifest.csv")
sample_file <- file.path("outputs", "audit", "wcad_usda_overlap", "potential_sample_post5_since2000.csv")
out_dir <- file.path("outputs", "audit", "farm_structure")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (f in c(wcad_manifest_file, sample_file)) if (!file.exists(f)) stop("Required input missing: ", f)

manifest <- read.csv(wcad_manifest_file, stringsAsFactors = FALSE, check.names = FALSE)
raw <- read.csv(manifest$selected_csv[[1]], stringsAsFactors = FALSE, check.names = FALSE)
sample <- read.csv(sample_file, stringsAsFactors = FALSE, check.names = FALSE)

required_raw <- c("Area", "Item", "Element", "Census Year", "Unit", "Value")
if (!all(required_raw %in% names(raw))) stop("WCAD schema changed; required columns missing.")
if (!all(c("wcad_country", "census_year") %in% names(sample))) stop("Candidate sample schema changed.")

raw$Area <- trimws(as.character(raw$Area))
raw$Item <- trimws(as.character(raw$Item))
raw$Element <- trimws(as.character(raw$Element))
raw$Unit <- trimws(as.character(raw$Unit))
raw$census_year <- suppressWarnings(as.integer(raw[["Census Year"]]))
raw$value_num <- suppressWarnings(as.numeric(raw$Value))
sample$wcad_country <- trimws(as.character(sample$wcad_country))
sample$census_year <- suppressWarnings(as.integer(sample$census_year))

sample_key <- paste(sample$wcad_country, sample$census_year, sep = "\u001f")
raw$key <- paste(raw$Area, raw$census_year, sep = "\u001f")
core <- raw[raw$key %in% sample_key & !is.na(raw$value_num), , drop = FALSE]
if (nrow(core) == 0L) stop("No WCAD rows overlap the candidate analytical sample.")

# Broad catalogue audit.
text <- tolower(paste(core$Item, core$Element, sep = " | "))
category <- rep(NA_character_, nrow(core))
category[grepl("parcel|fragment", text)] <- "fragmentation_parcels"
category[is.na(category) & grepl("size|area class|holding.*area|area.*holding", text)] <- "holding_size_distribution"
category[is.na(category) & grepl("holding", text)] <- "holdings_general"
category[is.na(category) & grepl("agricultural area|utili[sz]ed agricultural area|farm area", text)] <- "agricultural_area"

candidate <- core[!is.na(category), c("Area", "census_year", "Item", "Element", "Unit", "value_num"), drop = FALSE]
candidate$category <- category[!is.na(category)]
combo <- unique(candidate[, c("category", "Item", "Element", "Unit")])
coverage <- do.call(rbind, lapply(seq_len(nrow(combo)), function(i) {
  z <- combo[i, ]
  hit <- candidate$category == z$category & candidate$Item == z$Item & candidate$Element == z$Element & candidate$Unit == z$Unit
  d <- candidate[hit, , drop = FALSE]
  data.frame(category=z$category, item=z$Item, element=z$Element, unit=z$Unit,
             n_country_years=length(unique(paste(d$Area,d$census_year,sep="\u001f"))),
             n_countries=length(unique(d$Area)), stringsAsFactors=FALSE)
}))
coverage <- coverage[order(coverage$category, -coverage$n_countries, -coverage$n_country_years, coverage$item), , drop=FALSE]
write.csv(coverage, file.path(out_dir, "candidate_measure_coverage.csv"), row.names=FALSE, na="")

# Exact, pre-specified structural variables after inspecting the WCAD catalogue.
extract_exact <- function(item, element, unit) {
  d <- core[core$Item == item & core$Element == element & core$Unit == unit,
            c("Area", "census_year", "value_num"), drop=FALSE]
  k <- paste(d$Area, d$census_year, sep="\u001f")
  if (anyDuplicated(k)) stop("Duplicate WCAD key for exact measure: ", item, " / ", element, " / ", unit)
  d
}

avg <- extract_exact("Holdings", "Average area", "ha")
total_n <- extract_exact("Holdings", "Number", "No")
lt1 <- extract_exact("Holdings with land size 0-<1", "Number", "No")
one_lt2 <- extract_exact("Holdings with land size 1-<2", "Number", "No")

analytic <- sample
analytic$key <- paste(analytic$wcad_country, analytic$census_year, sep="\u001f")
lookup <- function(d) setNames(d$value_num, paste(d$Area, d$census_year, sep="\u001f"))
analytic$average_holding_area_ha <- unname(lookup(avg)[analytic$key])
analytic$total_holdings_n <- unname(lookup(total_n)[analytic$key])
analytic$holdings_lt1_n <- unname(lookup(lt1)[analytic$key])
analytic$holdings_1_lt2_n <- unname(lookup(one_lt2)[analytic$key])
analytic$small_holdings_share_lt2 <- ifelse(
  !is.na(analytic$total_holdings_n) & analytic$total_holdings_n > 0 &
    !is.na(analytic$holdings_lt1_n) & !is.na(analytic$holdings_1_lt2_n),
  100 * (analytic$holdings_lt1_n + analytic$holdings_1_lt2_n) / analytic$total_holdings_n,
  NA_real_
)

if (any(!is.na(analytic$small_holdings_share_lt2) &
        (analytic$small_holdings_share_lt2 < 0 | analytic$small_holdings_share_lt2 > 100))) {
  stop("Derived small-holdings share falls outside [0,100].")
}

structural_coverage <- data.frame(
  variable = c("average_holding_area_ha", "small_holdings_share_lt2", "both_structural_variables", "average_parcels_per_holding"),
  n_countries = c(
    sum(!is.na(analytic$average_holding_area_ha)),
    sum(!is.na(analytic$small_holdings_share_lt2)),
    sum(!is.na(analytic$average_holding_area_ha) & !is.na(analytic$small_holdings_share_lt2)),
    length(unique(core$Area[core$Item == "Parcels" & core$Element == "Average number of parcels per holding" & core$Unit == "No" & !is.na(core$value_num)]))
  ),
  stringsAsFactors = FALSE
)

write.csv(structural_coverage, file.path(out_dir, "structural_variable_coverage.csv"), row.names=FALSE, na="")
write.csv(analytic, file.path(out_dir, "candidate_sample_with_structure.csv"), row.names=FALSE, na="")

cat("FARM_STRUCTURE_COVERAGE_AUDIT_OK\n")
cat("Candidate sample countries:", nrow(sample), "\n")
print(structural_coverage, row.names=FALSE)
