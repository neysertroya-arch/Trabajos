# Build a double-anonymized Land Use Policy submission manuscript from the canonical article.
# Scientific results are not recalculated or edited here.
# This script is a deterministic editorial transform and must remain author-data free in its output.

source_file <- "manuscript/article.qmd"
out_file <- "submission/land-use-policy/article_lup_anonymous.qmd"

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

# The anonymous manuscript lives in a dedicated Quarto subproject two levels below repo root.
# Rewrite only generated publication-asset paths; bibliography is supplied by the local _quarto.yml.
body <- gsub("../outputs/publication/", "../../outputs/publication/", body, fixed = TRUE)

# Do not force tables onto new pages. Land Use Policy permits tables next to relevant text
# or on separate pages, so normal Word pagination is preferable and avoids large blank areas.

# Elsevier's current AI policy asks authors to describe AI assistance with research code in
# Methods when applicable. Insert a concise, non-causal transparency statement immediately
# before Results in the submission manuscript. The analytical code remains author-reviewed,
# executable in R, and subject to the repository's diagnostics and CI checks.
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
  "The source datasets used in this study are publicly available from the Food and Agriculture Organization of the United Nations, the U.S. Department of Agriculture Economic Research Service, and the World Bank, as cited in the manuscript. During double-anonymized peer review, the reproducible analysis code and the derived materials needed to reproduce the reported tables and figures are supplied as anonymized supplementary materials. A public repository is not identified during peer review in order to preserve author anonymity. Following acceptance, the authors will deposit the reproducibility materials in a public archival repository with a persistent identifier, subject to the journal's requirements and the terms governing redistribution of the original public-source data.",
  ""
)

body_anon <- append(body, declarations, after = ref_idx - 1L)
out <- c(yaml_anon, body_anon)
writeLines(out, out_file, useBytes = TRUE)

# Anonymity audit on the generated source. Extend this list if author metadata changes.
identifiers <- c(
  "Andrea Yolanda Jaramillo Castillo",
  "José Vicente Ordóñez Yaguache",
  "Jose Vicente Ordonez Yaguache",
  "Universidad Nacional de Loja",
  "josevicente1980",
  "0000-0002-3349-6714"
)
text <- paste(out, collapse = "\n")
hits <- identifiers[vapply(identifiers, function(z) grepl(z, text, fixed = TRUE), logical(1))]
if (length(hits)) stop("ANONYMITY_AUDIT_FAIL: ", paste(hits, collapse = "; "))

cat("LUP_ANONYMOUS_SOURCE_OK\n")
