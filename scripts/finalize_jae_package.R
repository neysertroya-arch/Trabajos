# ============================================================
# FINALIZACIÓN DEL PAQUETE — JOURNAL OF AGRICULTURAL ECONOMICS
# ============================================================
# Solo modifica elementos editoriales.
# No cambia datos, modelos, resultados ni referencias.
# ============================================================

dir_jae <- file.path(
  "submission",
  "journal-agricultural-economics"
)

title_file <- file.path(dir_jae, "title-page.md")
old_erae_docx <- file.path(dir_jae, "article_erae_anonymous.docx")

if (!dir.exists(dir_jae)) {
  stop("No existe la carpeta JAE.")
}

# ------------------------------------------------------------
# 1. TITLE PAGE JAE
# ------------------------------------------------------------

title_text <- c(
  "# Title page — Journal of Agricultural Economics",
  "",
  "**Article title:** Does Land Rental Prevalence Signal Market Effectiveness? Cross-Country Evidence on Agricultural TFP Growth",
  "",
  "## Authors",
  "",
  "**José Vicente Ordóñez Yaguache**  ",
  "Universidad Nacional de Loja, Av. Pío Jaramillo Alvarado y Av. Reinaldo Espinosa, postal code 110111, Loja, Ecuador  ",
  "Email: jose.ordonez@unl.edu.ec  ",
  "ORCID: 0000-0002-3349-6714",
  "",
  "**Andrea Yolanda Jaramillo Castillo**  ",
  "Universidad Nacional de Loja, Av. Pío Jaramillo Alvarado y Av. Reinaldo Espinosa, postal code 110111, Loja, Ecuador  ",
  "Email: andrea.y.jaramillo@unl.edu.ec  ",
  "ORCID: 0009-0003-5584-9845",
  "",
  "**JEL classification:** Q15; O47; Q12",
  "",
  "## Corresponding author",
  "",
  "**Corresponding author:** José Vicente Ordóñez Yaguache  ",
  "**Email:** jose.ordonez@unl.edu.ec  ",
  "**Postal address:** Universidad Nacional de Loja, Av. Pío Jaramillo Alvarado y Av. Reinaldo Espinosa, postal code 110111, Loja, Ecuador",
  "",
  "## Funding",
  "",
  "This research did not receive any specific grant from funding agencies in the public, commercial, or not-for-profit sectors.",
  "",
  "## Conflict of Interest",
  "",
  "The authors declare that they have no known competing financial interests or personal relationships that could have appeared to influence the work reported in this paper.",
  "",
  "## Data availability",
  "",
  "The source data used in this study are publicly available from the Food and Agriculture Organization of the United Nations, the U.S. Department of Agriculture Economic Research Service, and the World Bank. The analytical workflow was implemented in R, and the reproducibility materials will be made available in accordance with the journal's data and code requirements.",
  "",
  "## Artificial Intelligence disclosure",
  "",
  "OpenAI ChatGPT was used to support literature-search planning, manuscript organisation, language editing, and reproducibility-workflow planning and review. All bibliographic records and factual claims were checked against scholarly or official sources. All quantitative analyses were executed from source data in R, all analytical code was reviewed by the authors, and the authors take full responsibility for the manuscript.",
  "",
  "## CRediT author contributions",
  "",
  "**José Vicente Ordóñez Yaguache:** Conceptualization; Methodology; Software; Formal analysis; Data curation; Visualization; Writing – original draft; Writing – review & editing; Project administration.",
  "",
  "**Andrea Yolanda Jaramillo Castillo:** Conceptualization; Investigation; Writing – review & editing."
)

writeLines(
  title_text,
  title_file,
  useBytes = TRUE
)

# ------------------------------------------------------------
# 2. REMOVE STALE ERAE OUTPUT
# ------------------------------------------------------------

if (file.exists(old_erae_docx)) {
  unlink(old_erae_docx)
}

# ------------------------------------------------------------
# 3. BASIC PACKAGE VALIDATION
# ------------------------------------------------------------

required_title_terms <- c(
  "Journal of Agricultural Economics",
  "José Vicente Ordóñez Yaguache",
  "Andrea Yolanda Jaramillo Castillo",
  "Funding",
  "Conflict of Interest",
  "Data availability",
  "Artificial Intelligence disclosure",
  "CRediT author contributions"
)

txt <- paste(
  readLines(title_file, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

missing <- required_title_terms[
  !vapply(
    required_title_terms,
    function(z) grepl(z, txt, fixed = TRUE),
    logical(1)
  )
]

if (length(missing) > 0) {
  stop(
    "Missing title-page elements: ",
    paste(missing, collapse = "; ")
  )
}

cat("JAE_PACKAGE_FINALIZATION_OK\n")
cat("Title page updated:", title_file, "\n")
cat("Stale ERAE DOCX removed:", !file.exists(old_erae_docx), "\n")