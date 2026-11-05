# Build a double-anonymized Journal of Rural Studies submission manuscript from the canonical article.
# Scientific results are not recalculated or edited here.
# This is a deterministic journal-facing transform.

source_file <- "manuscript/article.qmd"
out_file <- "submission/journal-of-rural-studies/article_jrs_anonymous.qmd"

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

x <- readLines(source_file, warn = FALSE, encoding = "UTF-8")
front <- which(trimws(x) == "---")
if (length(front) < 2L) stop("Could not identify YAML front matter in ", source_file)
front_end <- front[2]

yaml <- x[seq_len(front_end)]
yaml_trim <- trimws(yaml)
author_start <- which(yaml_trim == "author:")
abstract_start <- which(startsWith(yaml_trim, "abstract:"))
if (length(author_start) != 1L || length(abstract_start) != 1L || author_start >= abstract_start) {
  stop("Unexpected author/abstract structure in YAML front matter")
}

yaml_anon <- yaml[-seq.int(author_start, abstract_start - 1L)]
body <- x[(front_end + 1L):length(x)]
body <- gsub("../outputs/publication/", "../../outputs/publication/", body, fixed = TRUE)

results_idx <- which(trimws(body) == "# 4. Results")
if (length(results_idx) != 1L) stop("Expected exactly one '# 4. Results' heading")
ai_methods <- c(
  "",
  "OpenAI ChatGPT was used as an assistive tool during development of the reproducibility workflow, including code-planning, code-review, and documentation support. All analytical code was reviewed by the authors, all quantitative analyses were executed from source data in R, and the reported results were verified through the predefined diagnostic and continuous-integration checks described in the reproducible workflow.",
  ""
)
body <- append(body, ai_methods, after = results_idx - 1L)

ref_idx <- which(trimws(body) == "# References")
if (length(ref_idx) != 1L) stop("Expected exactly one '# References' heading")

declarations <- c(
  "",
  "# Declaration of generative AI and AI-assisted technologies in the manuscript preparation process",
  "",
  "During preparation of this manuscript, the authors used OpenAI ChatGPT to support literature-search planning, manuscript organization, language editing, and reproducibility-workflow development. AI assistance with research code was limited to planning, review, and documentation support; all analytical code was reviewed by the authors, all quantitative analyses were executed from source data in R, and the resulting outputs were verified through predefined diagnostics and continuous-integration checks. All bibliographic records and factual claims used in the manuscript were checked against scholarly or official sources, and the authors reviewed and edited the resulting text and take full responsibility for the content of the article.",
  "",
  "# Data availability",
  "",
  "The source datasets used in this study are publicly available from the Food and Agriculture Organization of the United Nations, the U.S. Department of Agriculture Economic Research Service, and the World Bank, as cited in the manuscript. Reproducibility materials will be deposited in a public archival repository with a persistent identifier in accordance with the journal's requirements, subject to the terms governing redistribution of the original public-source data.",
  ""
)

body_anon <- append(body, declarations, after = ref_idx - 1L)
out <- c(yaml_anon, body_anon)
writeLines(out, out_file, useBytes = TRUE)

identifiers <- c(
  "Andrea Yolanda Jaramillo Castillo",
  "José Vicente Ordóñez Yaguache",
  "Jose Vicente Ordonez Yaguache",
  "Universidad Nacional de Loja",
  "josevicente1980",
  "0000-0002-3349-6714",
  "0009-0003-5584-9845"
)
text <- paste(out, collapse = "\n")
hits <- identifiers[vapply(identifiers, function(z) grepl(z, text, fixed = TRUE), logical(1))]
if (length(hits)) stop("ANONYMITY_AUDIT_FAIL: ", paste(hits, collapse = "; "))

cat("JRS_ANONYMOUS_SOURCE_OK\n")
