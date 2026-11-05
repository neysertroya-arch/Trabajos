# 04_harmonize.R ---------------------------------------------------------
# Harmonize WCAD rental-area percentage observations with USDA ERS
# International Agricultural Productivity. Feasibility/sample audit only:
# no econometric model is estimated here.

wcad_file <- file.path("outputs", "audit", "wcad_comparability", "rental_area_percent_country_year.csv")
wcad_manifest_file <- file.path("data_raw", "faostat_wcad", "wcad_download_manifest.csv")
usda_manifest_file <- file.path("data_raw", "usda_tfp", "usda_tfp_download_manifest.csv")
out_dir <- file.path("outputs", "audit", "wcad_usda_overlap")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (f in c(wcad_file, wcad_manifest_file, usda_manifest_file)) {
  if (!file.exists(f)) stop("Required input missing: ", f)
}

wcad <- read.csv(wcad_file, stringsAsFactors = FALSE, check.names = FALSE)
wcad_manifest <- read.csv(wcad_manifest_file, stringsAsFactors = FALSE, check.names = FALSE)
wcad_raw <- read.csv(wcad_manifest$selected_csv[[1]], stringsAsFactors = FALSE, check.names = FALSE)
usda_manifest <- read.csv(usda_manifest_file, stringsAsFactors = FALSE, check.names = FALSE)
usda <- read.csv(usda_manifest$downloaded_file[[1]], stringsAsFactors = FALSE, check.names = FALSE)

normalize_country <- function(x) {
  z <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  z <- tolower(trimws(z))
  z <- gsub("&", " and ", z, fixed = TRUE)
  z <- gsub("[^a-z0-9]+", " ", z)
  z <- gsub("\\bthe\\b", " ", z)
  z <- gsub("\\s+", " ", z)
  trimws(z)
}

canonical_country <- function(x) {
  z <- normalize_country(x)
  aliases <- c(
    "bolivia plurinational state of" = "bolivia",
    "brunei darussalam" = "brunei",
    "cabo verde" = "cape verde",
    "china mainland" = "china",
    "congo" = "congo republic",
    "congo democratic republic of" = "congo dr",
    "democratic republic of congo" = "congo dr",
    "democratic republic of the congo" = "congo dr",
    "cote d ivoire" = "cote divoire",
    "czech republic" = "czechia",
    "iran islamic republic of" = "iran",
    "korea democratic people s republic of" = "korea dpr",
    "democratic people s republic of korea" = "korea dpr",
    "korea republic of" = "korea republic",
    "republic of korea" = "korea republic",
    "lao people s democratic republic" = "laos",
    "micronesia federated states of" = "micronesia",
    "moldova republic of" = "moldova",
    "republic of moldova" = "moldova",
    "netherlands kingdom of" = "netherlands",
    "north macedonia" = "macedonia",
    "palestine" = "west bank and gaza",
    "russian federation" = "russia",
    "syrian arab republic" = "syria",
    "taiwan province of china" = "taiwan",
    "turkiye" = "turkey",
    "tanzania united republic of" = "tanzania",
    "united republic of tanzania" = "tanzania",
    "united kingdom of great britain and northern ireland" = "united kingdom",
    "united states of america" = "united states",
    "venezuela bolivarian republic of" = "venezuela",
    "viet nam" = "vietnam",
    "west bank and gaza strip" = "west bank and gaza"
  )
  hit <- match(z, names(aliases))
  z[!is.na(hit)] <- unname(aliases[hit[!is.na(hit)]])
  z
}

# ---- WCAD stable identifier map ---------------------------------------
required_wcad_raw <- c("Area", "Area Code")
if (!all(required_wcad_raw %in% names(wcad_raw))) {
  stop("WCAD raw file does not contain Area and Area Code.")
}

area_map <- unique(data.frame(
  wcad_country = trimws(as.character(wcad_raw$Area)),
  fao_code = suppressWarnings(as.integer(wcad_raw[["Area Code"]])),
  stringsAsFactors = FALSE
))
area_map <- area_map[!is.na(area_map$wcad_country) & nzchar(area_map$wcad_country), , drop = FALSE]

wcad$wcad_country <- trimws(as.character(wcad$country))
wcad$census_year <- suppressWarnings(as.integer(wcad$census_year))
wcad$rental_area_share <- suppressWarnings(as.numeric(wcad$value))
wcad$fao_code <- area_map$fao_code[match(wcad$wcad_country, area_map$wcad_country)]
wcad$name_key <- canonical_country(wcad$wcad_country)
wcad <- wcad[!is.na(wcad$census_year) & !is.na(wcad$rental_area_share), , drop = FALSE]

# ---- USDA exact schema -------------------------------------------------
required_usda <- c("FAO", "ISO3", "Country/territory", "Year", "Variable", "Value")
missing_usda <- setdiff(required_usda, names(usda))
if (length(missing_usda) > 0L) {
  stop("USDA file lacks expected columns: ", paste(missing_usda, collapse = ", "))
}

variable_text <- tolower(trimws(as.character(usda$Variable)))
is_tfp <- variable_text == "ag tfp" |
  variable_text == "tfp" |
  grepl("total factor productivity", variable_text) |
  (grepl("(^|[^a-z])tfp([^a-z]|$)", variable_text) &
     !grepl("growth|change|rate", variable_text))

usda_tfp <- usda[is_tfp, , drop = FALSE]
if (nrow(usda_tfp) == 0L) {
  stop("No USDA Ag TFP index observations were detected. Available Variable labels include: ",
       paste(utils::head(unique(as.character(usda$Variable)), 20), collapse = " | "))
}

usda_tfp$usda_country <- trimws(as.character(usda_tfp[["Country/territory"]]))
usda_tfp$fao_code <- suppressWarnings(as.integer(usda_tfp$FAO))
usda_tfp$iso3 <- trimws(as.character(usda_tfp$ISO3))
usda_tfp$name_key <- canonical_country(usda_tfp$usda_country)
usda_tfp$tfp_year <- suppressWarnings(as.integer(usda_tfp$Year))
usda_tfp$tfp_value <- suppressWarnings(as.numeric(usda_tfp$Value))

# USDA also contains regional/income/world aggregates. They have no country
# name and must never enter a country-year key.
usda_tfp <- usda_tfp[
  !is.na(usda_tfp$usda_country) & nzchar(usda_tfp$usda_country) &
    !is.na(usda_tfp$tfp_year) & !is.na(usda_tfp$tfp_value) & usda_tfp$tfp_value > 0,
  , drop = FALSE
]
if (nrow(usda_tfp) == 0L) stop("No valid national USDA TFP observations remain after excluding aggregates.")

# A stable FAO code is primary. Name matching is retained only as a fallback
# for records lacking a usable code.
usda_tfp$id_key <- ifelse(
  !is.na(usda_tfp$fao_code),
  paste0("fao:", usda_tfp$fao_code),
  paste0("name:", usda_tfp$name_key)
)

key <- paste(usda_tfp$id_key, usda_tfp$tfp_year, sep = "\u001f")
splits <- split(seq_len(nrow(usda_tfp)), key)
tfp_unique <- do.call(rbind, lapply(splits, function(idx) {
  vals <- unique(usda_tfp$tfp_value[idx])
  data.frame(
    usda_country = usda_tfp$usda_country[idx[[1]]],
    fao_code = usda_tfp$fao_code[idx[[1]]],
    iso3 = usda_tfp$iso3[idx[[1]]],
    name_key = usda_tfp$name_key[idx[[1]]],
    id_key = usda_tfp$id_key[idx[[1]]],
    tfp_year = usda_tfp$tfp_year[idx[[1]]],
    tfp_value = if (length(vals) == 1L) vals[[1]] else NA_real_,
    conflicting_duplicate = length(vals) > 1L,
    stringsAsFactors = FALSE
  )
}))
rownames(tfp_unique) <- NULL

conflicting_tfp <- tfp_unique[tfp_unique$conflicting_duplicate, , drop = FALSE]
write.csv(conflicting_tfp, file.path(out_dir, "conflicting_usda_country_years.csv"), row.names = FALSE, na = "")
if (nrow(conflicting_tfp) > 0L) {
  stop("USDA contains conflicting national TFP values for ", nrow(conflicting_tfp), " stable country-year key(s).")
}
tfp_unique <- tfp_unique[!is.na(tfp_unique$tfp_value), , drop = FALSE]

lookup_row <- function(fao_code, name_key, yr) {
  hit_code <- !is.na(fao_code) & !is.na(tfp_unique$fao_code) &
    tfp_unique$fao_code == fao_code & tfp_unique$tfp_year == yr
  if (any(hit_code)) return(which(hit_code)[[1]])

  hit_name <- tfp_unique$name_key == name_key & tfp_unique$tfp_year == yr
  if (any(hit_name)) return(which(hit_name)[[1]])
  NA_integer_
}

lookup_value <- function(fao_code, name_key, yr) {
  i <- lookup_row(fao_code, name_key, yr)
  if (is.na(i)) return(NA_real_)
  tfp_unique$tfp_value[[i]]
}

lookup_country <- function(fao_code, name_key) {
  hit_code <- !is.na(fao_code) & !is.na(tfp_unique$fao_code) & tfp_unique$fao_code == fao_code
  if (any(hit_code)) return(tfp_unique$usda_country[which(hit_code)[[1]]])
  hit_name <- tfp_unique$name_key == name_key
  if (any(hit_name)) return(tfp_unique$usda_country[which(hit_name)[[1]]])
  NA_character_
}

lookup_method <- function(fao_code, name_key) {
  hit_code <- !is.na(fao_code) & !is.na(tfp_unique$fao_code) & tfp_unique$fao_code == fao_code
  if (any(hit_code)) return("FAO_code")
  if (any(tfp_unique$name_key == name_key)) return("name_fallback")
  "unmatched"
}

# ---- Window feasibility ------------------------------------------------
overlap <- wcad
overlap$usda_country <- mapply(lookup_country, overlap$fao_code, overlap$name_key, USE.NAMES = FALSE)
overlap$match_method <- mapply(lookup_method, overlap$fao_code, overlap$name_key, USE.NAMES = FALSE)
overlap$country_matched <- overlap$match_method != "unmatched"

overlap$tfp_tminus5 <- mapply(lookup_value, overlap$fao_code, overlap$name_key, overlap$census_year - 5L)
overlap$tfp_t0 <- mapply(lookup_value, overlap$fao_code, overlap$name_key, overlap$census_year)
overlap$tfp_tplus3 <- mapply(lookup_value, overlap$fao_code, overlap$name_key, overlap$census_year + 3L)
overlap$tfp_tplus5 <- mapply(lookup_value, overlap$fao_code, overlap$name_key, overlap$census_year + 5L)

overlap$post3_available <- !is.na(overlap$tfp_t0) & !is.na(overlap$tfp_tplus3)
overlap$post5_available <- !is.na(overlap$tfp_t0) & !is.na(overlap$tfp_tplus5)
overlap$pre5_available <- !is.na(overlap$tfp_tminus5) & !is.na(overlap$tfp_t0)
overlap$pre5_post5_available <- overlap$pre5_available & overlap$post5_available

overlap$tfp_growth_post3 <- ifelse(overlap$post3_available, (log(overlap$tfp_tplus3) - log(overlap$tfp_t0)) / 3, NA_real_)
overlap$tfp_growth_post5 <- ifelse(overlap$post5_available, (log(overlap$tfp_tplus5) - log(overlap$tfp_t0)) / 5, NA_real_)
overlap$tfp_growth_pre5 <- ifelse(overlap$pre5_available, (log(overlap$tfp_t0) - log(overlap$tfp_tminus5)) / 5, NA_real_)

tfp_min_year <- min(tfp_unique$tfp_year, na.rm = TRUE)
tfp_max_year <- max(tfp_unique$tfp_year, na.rm = TRUE)
overlap$exclusion_reason_post5 <- ifelse(
  !overlap$country_matched, "country_not_matched",
  ifelse(overlap$census_year < tfp_min_year, "census_before_tfp_series",
    ifelse(overlap$census_year + 5L > tfp_max_year, "insufficient_post_census_years",
      ifelse(is.na(overlap$tfp_t0), "tfp_missing_at_census_year",
        ifelse(is.na(overlap$tfp_tplus5), "tfp_missing_at_plus5", "eligible")
      )
    )
  )
)

write.csv(overlap, file.path(out_dir, "wcad_usda_overlap_by_census.csv"), row.names = FALSE, na = "")

unmatched <- unique(overlap[!overlap$country_matched, c("wcad_country", "fao_code", "name_key")])
write.csv(unmatched, file.path(out_dir, "unmatched_wcad_countries.csv"), row.names = FALSE, na = "")

match_methods <- as.data.frame(table(unique(overlap[, c("wcad_country", "match_method")])$match_method), stringsAsFactors = FALSE)
names(match_methods) <- c("match_method", "n_countries")
write.csv(match_methods, file.path(out_dir, "country_match_methods.csv"), row.names = FALSE, na = "")

reason_table <- as.data.frame(table(overlap$exclusion_reason_post5), stringsAsFactors = FALSE)
names(reason_table) <- c("reason", "n_country_years")
write.csv(reason_table, file.path(out_dir, "post5_exclusion_reasons.csv"), row.names = FALSE, na = "")

latest_eligible <- function(df, flag, cutoff = -Inf) {
  d <- df[df[[flag]] & df$census_year >= cutoff, , drop = FALSE]
  if (nrow(d) == 0L) return(d)
  d <- d[order(d$fao_code, d$name_key, -d$census_year), , drop = FALSE]
  id <- ifelse(!is.na(d$fao_code), paste0("fao:", d$fao_code), paste0("name:", d$name_key))
  d[!duplicated(id), , drop = FALSE]
}

sample_post3_2000 <- latest_eligible(overlap, "post3_available", 2000)
sample_post5_2000 <- latest_eligible(overlap, "post5_available", 2000)
sample_post5_2010 <- latest_eligible(overlap, "post5_available", 2010)
sample_prepost_2000 <- latest_eligible(overlap, "pre5_post5_available", 2000)

write.csv(sample_post3_2000, file.path(out_dir, "potential_sample_post3_since2000.csv"), row.names = FALSE, na = "")
write.csv(sample_post5_2000, file.path(out_dir, "potential_sample_post5_since2000.csv"), row.names = FALSE, na = "")
write.csv(sample_post5_2010, file.path(out_dir, "potential_sample_post5_since2010.csv"), row.names = FALSE, na = "")
write.csv(sample_prepost_2000, file.path(out_dir, "potential_sample_pre5_post5_since2000.csv"), row.names = FALSE, na = "")

country_id <- ifelse(!is.na(overlap$fao_code), paste0("fao:", overlap$fao_code), paste0("name:", overlap$name_key))
matched_id <- unique(country_id[overlap$country_matched])

summary <- data.frame(
  metric = c(
    "wcad_country_years",
    "wcad_countries",
    "countries_matched_to_usda",
    "unmatched_wcad_countries",
    "countries_matched_by_fao_code",
    "countries_matched_by_name_fallback",
    "usda_tfp_min_year",
    "usda_tfp_max_year",
    "country_years_post3_available",
    "country_years_post5_available",
    "country_years_pre5_post5_available",
    "countries_post3_since2000_latest_per_country",
    "countries_post5_since2000_latest_per_country",
    "countries_post5_since2010_latest_per_country",
    "countries_pre5_post5_since2000_latest_per_country",
    "conflicting_usda_country_year_keys"
  ),
  value = c(
    nrow(overlap),
    length(unique(country_id)),
    length(matched_id),
    nrow(unmatched),
    sum(match_methods$n_countries[match_methods$match_method == "FAO_code"]),
    sum(match_methods$n_countries[match_methods$match_method == "name_fallback"]),
    tfp_min_year,
    tfp_max_year,
    sum(overlap$post3_available),
    sum(overlap$post5_available),
    sum(overlap$pre5_post5_available),
    nrow(sample_post3_2000),
    nrow(sample_post5_2000),
    nrow(sample_post5_2010),
    nrow(sample_prepost_2000),
    nrow(conflicting_tfp)
  ),
  stringsAsFactors = FALSE
)
write.csv(summary, file.path(out_dir, "overlap_summary.csv"), row.names = FALSE, na = "")

cat("WCAD_USDA_OVERLAP_AUDIT_OK\n")
print(summary, row.names = FALSE)
cat("\nCountry match methods:\n")
print(match_methods, row.names = FALSE)
cat("\nPost-5 exclusion reasons:\n")
print(reason_table, row.names = FALSE)
