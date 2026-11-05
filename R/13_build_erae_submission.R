# Build the European Review of Agricultural Economics anonymous submission source.
# Scientific results are not recalculated or edited here.

source_file <- "manuscript/article.qmd"
out_file <- "submission/european-review-agricultural-economics/article_erae_anonymous.qmd"
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

# Regenerate journal-facing tables, figures, and Results prose from approved analytical outputs.
# This script does not re-estimate inferential models.
source("R/08_publication_outputs.R")

x <- readLines(source_file, warn = FALSE, encoding = "UTF-8")
front <- which(trimws(x) == "---")
if (length(front) < 2L) stop("YAML front matter not found")
front_end <- front[2]

yaml <- x[seq_len(front_end)]
yt <- trimws(yaml)
author_start <- which(yt == "author:")
abstract_start <- which(startsWith(yt, "abstract:"))
keywords_start <- which(startsWith(yt, "keywords:"))
if (length(author_start) != 1L || length(abstract_start) != 1L || length(keywords_start) != 1L) {
  stop("Unexpected YAML structure")
}

yaml_anon <- yaml[-seq.int(author_start, abstract_start - 1L)]
yt2 <- trimws(yaml_anon)
a2 <- which(startsWith(yt2, "abstract:"))
k2 <- which(startsWith(yt2, "keywords:"))

abstract_erae <- c(
  "abstract: |",
  "  Land rental can reallocate land toward more productive operators, but national rental prevalence may not capture market effectiveness. Linking FAO agricultural-census indicators for 65 countries to USDA agricultural TFP and World Bank controls, we estimate whether rental-area share predicts five-year post-census TFP growth. A 10-percentage-point higher rental share is associated with -0.0011 annualised log TFP growth (HC3 p = 0.598); the interaction with small-holding prevalence is also imprecise. Results are stable across predefined robustness checks. Aggregate rental prevalence alone is therefore not a reliable cross-country predictor of subsequent agricultural TFP growth."
)
erae_title <- "Land Rental, Farm Structure and Agricultural TFP Growth: Cross-Country Census Evidence"
title_idx <- which(startsWith(trimws(yaml_anon), "title:"))
if (length(title_idx) != 1L) stop("Title field not found in YAML")
yaml_anon[title_idx] <- paste0('title: "', erae_title, '"')

yaml_anon <- c(
  yaml_anon[seq_len(a2 - 1L)],
  abstract_erae,
  "keywords: [land rental, land tenure, agricultural productivity, total factor productivity, farm structure]",
  yaml_anon[(k2 + 1L):length(yaml_anon)]
)

body <- x[(front_end + 1L):length(x)]
body <- gsub("../outputs/publication/", "../../outputs/publication/", body, fixed = TRUE)
body <- sub("# 2. Conceptual framework", "# 2. Materials and methods\n\n## Conceptual framework", body, fixed = TRUE)
body <- sub("# 3. Data and empirical strategy", "## Data and empirical strategy", body, fixed = TRUE)
body <- sub("# 4. Results", "# 3. Results", body, fixed = TRUE)
body <- sub("# 5. Discussion and implications for rural policy", "# 4. Discussion and policy implications", body, fixed = TRUE)
body <- sub("# 6. Conclusion", "# 5. Conclusion", body, fixed = TRUE)
body <- sub(
  "Framed in rural-studies terms, the paper evaluates whether a readily observable feature of rural land institutions—rental prevalence—contains information about subsequent structural productivity dynamics beyond what can be learned from farm-level transactions alone.",
  "From an applied agricultural-economics perspective, the paper evaluates whether a readily observable feature of land-market activity—rental prevalence—contains information about subsequent productivity dynamics beyond what can be learned from farm-level transactions alone.",
  body, fixed = TRUE
)
body <- sub(
  "This distinction is especially relevant for rural studies because land rental is simultaneously an economic transaction, a tenure institution, and a mechanism through which rural households adjust operational scale, labor allocation, and livelihood strategies.",
  "This distinction is especially relevant for agricultural economics because land rental is simultaneously an economic transaction, a tenure institution, and a mechanism through which farm households adjust operational scale, labor allocation, and livelihood strategies.",
  body, fixed = TRUE
)
body <- sub("The rural-policy implication is correspondingly specific.", "The policy implication is correspondingly specific.", body, fixed = TRUE)
body <- sub(
  "The practical implication for rural policy is to measure how rental markets work, not only how much land is rented.",
  "The practical implication for agricultural and land policy is to measure how rental markets work, not only how much land is rented.",
  body, fixed = TRUE
)

# ERAE requires British English. Apply only to journal-facing prose before references.
ref_pos_for_language <- which(trimws(body) == "# References")
if (length(ref_pos_for_language) != 1L) stop("References heading not found for language pass")
prose_idx <- seq_len(ref_pos_for_language - 1L)
body[prose_idx] <- gsub("\\blabor\\b", "labour", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bLabor\\b", "Labour", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bcentered\\b", "centred", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bCentering\\b", "Centring", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bfavorable\\b", "favourable", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bharmonized\\b", "harmonised", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bharmonization\\b", "harmonisation", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bequalize\\b", "equalise", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bequalization\\b", "equalisation", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bmechanization\\b", "mechanisation", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bnormalized\\b", "normalised", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\brecognizing\\b", "recognising", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bannualized\\b", "annualised", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bemphasizes\\b", "emphasises", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bemphasized\\b", "emphasised", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bemphasizing\\b", "emphasising", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bsummarize\\b", "summarise", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bsummarizes\\b", "summarises", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bformalization\\b", "formalisation", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bcommercialization\\b", "commercialisation", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bregularization\\b", "regularisation", body[prose_idx], ignore.case = FALSE)
body[prose_idx] <- gsub("\\bcenters\\b", "centres", body[prose_idx], ignore.case = FALSE)

results_idx <- which(trimws(body) == "# 3. Results")
if (length(results_idx) != 1L) stop("Results heading not found")
ai <- c(
  "",
  "**AI-assisted tools disclosure.** During preparation of this manuscript, the authors used OpenAI ChatGPT to support literature-search planning, manuscript organisation, language editing, and reproducibility-workflow planning and review. All bibliographic records and factual claims were checked against scholarly or official sources. All quantitative analyses were executed from source data in R, all analytical code was reviewed by the authors, and the authors take full responsibility for the manuscript.",
  ""
)
body <- append(body, ai, after = results_idx - 1L)

# ERAE requests alt text directly under each figure legend.
fig1_idx <- grep("^!\\[Unconditional relationship between agricultural land-rental prevalence", body)
fig2_idx <- grep("^!\\[Covariate-adjusted relationship implied directly", body)
if (length(fig1_idx) != 1L || length(fig2_idx) != 1L) stop("Figure markers not uniquely found for alt text")
body <- append(
  body,
  c("", "Alt text: Scatter plot of 65 countries with agricultural area rented from others on the horizontal axis and five-year annualised agricultural TFP growth on the vertical axis. Country observations are widely dispersed around a slightly downward-sloping fitted line, indicating no clear unconditional relationship.", ""),
  after = fig1_idx
)
fig2_idx <- grep("^!\\[Covariate-adjusted relationship implied directly", body)
body <- append(
  body,
  c("", "Alt text: Line plot of covariate-adjusted five-year annualised agricultural TFP growth against agricultural area rented from others, with the remaining covariates held at their sample means. The fitted relationship declines modestly across the observed rental-share range.", ""),
  after = fig2_idx
)

ref_idx <- which(trimws(body) == "# References")
if (length(ref_idx) != 1L) stop("References heading not found")
data_availability <- c(
  "",
  "# Data availability",
  "",
  "The source datasets used in this study are publicly available from the Food and Agriculture Organization of the United Nations, the U.S. Department of Agriculture Economic Research Service, and the World Bank, as cited in the manuscript. Reproducibility materials will be deposited in a public archival repository with a persistent identifier in accordance with the journal's requirements, subject to the terms governing redistribution of the original public-source data.",
  ""
)
body <- append(body, data_availability, after = ref_idx - 1L)

intro_idx <- which(trimws(body) == "# 1. Introduction")
body <- append(body, c("", "**Keywords:** land rental; land tenure; agricultural productivity; total factor productivity; farm structure", "", "**JEL classification:** Q15; O47; Q12", ""), after = intro_idx - 1L)

out <- c(yaml_anon, body)
writeLines(out, out_file, useBytes = TRUE)

abstract_text <- paste(abstract_erae[-1], collapse = " ")
abstract_words <- length(strsplit(gsub("[^[:alnum:]'-]+", " ", abstract_text), "\\s+")[[1]])
if (abstract_words > 100L) stop("ERAE abstract exceeds 100 words")

identifiers <- c(
  "José Vicente Ordóñez Yaguache",
  "Andrea Yolanda Jaramillo Castillo",
  "Universidad Nacional de Loja",
  "josevicente1980",
  "0000-0002-3349-6714",
  "0009-0003-5584-9845"
)
text <- paste(out, collapse = "\n")
hits <- identifiers[vapply(identifiers, function(z) grepl(z, text, fixed = TRUE), logical(1))]
if (length(hits)) stop("ANONYMITY_AUDIT_FAIL: ", paste(hits, collapse = "; "))
if (grepl("Journal of Rural Studies|Land Use Policy", text)) stop("PRIOR_JOURNAL_STRING_FAIL")

cat("ERAE_ANONYMOUS_SOURCE_OK\n")
