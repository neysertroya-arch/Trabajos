# scripts/adapt_jae.R
# Adaptación editorial reproducible del manuscrito para
# Journal of Agricultural Economics.
# No modifica datos, modelos ni resultados cuantitativos.

article <- file.path(
  "submission",
  "journal-agricultural-economics",
  "article_jae_anonymous.qmd"
)

cover <- file.path(
  "submission",
  "journal-agricultural-economics",
  "cover-letter.md"
)

quarto <- file.path(
  "submission",
  "journal-agricultural-economics",
  "_quarto.yml"
)

for (f in c(article, cover, quarto)) {
  if (!file.exists(f)) stop("Missing required file: ", f)
}

x <- readLines(article, warn = FALSE, encoding = "UTF-8")
txt <- paste(x, collapse = "\n")

# ------------------------------------------------------------------
# 1. TITLE
# ------------------------------------------------------------------

txt <- sub(
  'title: "Land Rental Prevalence, Farm Structure and Agricultural TFP Growth: Cross-Country Evidence from Agricultural Censuses"',
  'title: "Does Land Rental Prevalence Signal Market Effectiveness? Cross-Country Evidence on Agricultural TFP Growth"',
  txt,
  fixed = TRUE
)

# ------------------------------------------------------------------
# 2. ABSTRACT
# ------------------------------------------------------------------

old_abstract <- paste0(
  "abstract: |\n",
  "  Land rental can reallocate land toward more productive operators, but national rental prevalence may not capture market effectiveness. Linking FAO agricultural-census indicators for 65 countries to USDA agricultural TFP and World Bank controls, we estimate whether rental-area share predicts five-year post-census TFP growth. A 10-percentage-point higher rental share is associated with -0.0011 annualised log TFP growth (HC3 p = 0.598); the interaction with small-holding prevalence is also imprecise. Results are stable across predefined robustness checks. Aggregate rental prevalence alone is therefore not a reliable cross-country predictor of subsequent agricultural TFP growth."
)

new_abstract <- paste0(
  "abstract: |\n",
  "  Land-rental markets can improve agricultural allocation when land moves toward more productive operators, but the prevalence of rental transactions may not measure how effectively that reallocation occurs. This article tests whether harmonised national land-rental prevalence contains information about subsequent agricultural productivity growth. We link FAO agricultural-census indicators for 65 countries to USDA agricultural total factor productivity data and World Bank development controls. A 10-percentage-point higher rental-area share is associated with a -0.0011 change in annualised five-year post-census TFP growth (HC3 p = 0.598), while the interaction with small-holding prevalence is also imprecisely estimated. The interpretation is stable across predefined checks using alternative horizons, rental measures, influence diagnostics, a growth-acceleration outcome and an exploratory repeated-census fixed-effects specification. The evidence therefore distinguishes rental-market activity from rental-market effectiveness: countries with more rental activity do not systematically exhibit faster subsequent productivity growth. The findings suggest that rental prevalence alone is an insufficient international indicator of how effectively land markets reallocate agricultural resources."
)

if (!grepl(old_abstract, txt, fixed = TRUE)) {
  stop("Expected ERAE abstract not found; no changes written.")
}

txt <- sub(old_abstract, new_abstract, txt, fixed = TRUE)

# ------------------------------------------------------------------
# 3. INTRODUCTION: replace final paragraph
# ------------------------------------------------------------------

intro_start <- paste0(
  "Against this background, this article asks a deliberately narrower aggregate question:"
)

intro_end <- "\n\n# 2. Materials and methods"

p1 <- regexpr(intro_start, txt, fixed = TRUE)
p2 <- regexpr(intro_end, txt, fixed = TRUE)

if (p1 < 1 || p2 < 1 || p2 <= p1) {
  stop("Introduction replacement boundaries not found.")
}

new_intro <- paste0(
  "Against this background, this article asks a different question from most of the existing land-rental literature: does the national prevalence of agricultural rental arrangements provide an informative aggregate signal of the effectiveness of land reallocation? The distinction matters because microeconomic evidence can establish that rental transactions improve allocation under particular institutional conditions without implying that countries with more rental activity necessarily experience faster productivity growth.\n\n",
  "We address this question by linking harmonised agricultural-census indicators of rental prevalence and farm structure to subsequent national agricultural TFP growth. To our knowledge, the existing literature has not established whether internationally comparable census measures of rental prevalence contain a systematic signal about subsequent aggregate productivity dynamics. The analysis therefore provides a cross-country boundary test of the extent to which a well-established microeconomic mechanism survives aggregation into an observable national indicator.\n\n",
  "The contribution is threefold. First, the paper shifts the empirical object from the productivity consequences of individual rental transactions to the informational content of aggregate rental prevalence. Second, it distinguishes rental-market activity from rental-market effectiveness: a larger share of rented agricultural area need not imply that land moves toward higher-productivity operators or that contractual institutions support productivity-enhancing investment. Third, it evaluates this proposition using harmonised census information, subsequent TFP growth and predefined robustness checks rather than specifications selected according to statistical significance. The analysis remains explicitly associational and does not identify the causal effect of rental-market reforms.\n\n",
  "This distinction has a direct policy implication. International comparisons often rely on observable measures of market participation because institutional effectiveness is much harder to measure consistently across countries. If rental prevalence does not contain a stable aggregate productivity signal, the relevant conclusion is not that rental markets are unimportant, but that transaction volume alone is an insufficient indicator of how effectively those markets reallocate agricultural land."
)

prefix <- substr(txt, 1, p1 - 1)
suffix <- substr(txt, p2, nchar(txt))

txt <- paste0(prefix, new_intro, suffix)

# ------------------------------------------------------------------
# 4. DISCUSSION: replace opening paragraphs only
# ------------------------------------------------------------------

disc_start <- "# 4. Discussion and policy implications\n\n"
disc_pos <- regexpr(disc_start, txt, fixed = TRUE)

if (disc_pos < 1) stop("Discussion heading not found.")

after_disc <- disc_pos + attr(disc_pos, "match.length")

# Preserve discussion from the paragraph beginning
# "A first explanation..." onward.
keep_marker <- "A first explanation for the weak aggregate signal"
keep_pos <- regexpr(keep_marker, txt, fixed = TRUE)

if (keep_pos < 1 || keep_pos <= after_disc) {
  stop("Discussion preservation marker not found.")
}

new_disc <- paste0(
  "# 4. Discussion and policy implications\n\n",
  "The results identify an important aggregation boundary in the land-market literature. Across the harmonised international sample, national rental prevalence does not contain a stable signal about subsequent agricultural TFP growth. This finding does not contradict evidence that land-rental markets can improve allocation within particular institutional settings. Rather, it shows that the prevalence of rental transactions and the effectiveness of those transactions are empirically distinct objects.\n\n",
  "This distinction helps reconcile the present results with microeconomic and quantitative evidence showing potentially important productivity effects of land-market frictions. Studies based on farm-level variation, institutional reforms or allocation models measure mechanisms and distortions more directly than an aggregate rental share. Our cross-country analysis addresses a complementary question: whether the observable volume of rental activity can substitute for those richer measures when comparing countries. The results suggest that it cannot reliably do so.\n\n",
  "The policy implication is therefore more precise than a statement about whether rental markets work. Rental prevalence should not be interpreted mechanically as a performance indicator of land-market institutions. Similar national rental shares can arise under different combinations of tenure security, enforcement, contract duration, transaction costs, credit constraints and tenant selection. What matters for productivity is not simply whether land changes operational control, but whether the institutional environment enables land to move toward higher-return uses while preserving incentives for investment.\n\n"
)

txt <- paste0(
  substr(txt, 1, disc_pos - 1),
  new_disc,
  substr(txt, keep_pos, nchar(txt))
)

# ------------------------------------------------------------------
# 5. PRACTITIONER POINTS
# ------------------------------------------------------------------

practitioner <- paste0(
  "\n\n## Practitioner points\n\n",
  "- National land-rental prevalence should not be treated as a direct indicator of land-market effectiveness.\n",
  "- Similar levels of rental activity can coexist with very different institutional conditions and allocative outcomes.\n",
  "- Cross-country monitoring should complement rental-prevalence measures with indicators of contract security, market access, transaction costs and the direction of land reallocation.\n"
)

# Insert immediately before Data availability.
data_marker <- "\n# Data availability"
data_pos <- regexpr(data_marker, txt, fixed = TRUE)

if (data_pos < 1) stop("Data availability section not found.")

txt <- paste0(
  substr(txt, 1, data_pos - 1),
  practitioner,
  substr(txt, data_pos, nchar(txt))
)

writeLines(strsplit(txt, "\n", fixed = TRUE)[[1]], article, useBytes = TRUE)

# ------------------------------------------------------------------
# 6. COVER LETTER
# ------------------------------------------------------------------

cover_text <- paste0(
  "Dear Editors,\n\n",
  "Please consider our manuscript, **“Does Land Rental Prevalence Signal Market Effectiveness? Cross-Country Evidence on Agricultural TFP Growth,”** for publication in the *Journal of Agricultural Economics*.\n\n",
  "The manuscript addresses a gap between the large microeconomic literature on land-rental markets and the use of aggregate land-market indicators in international agricultural analysis. Rather than estimating another country-specific effect of rental participation, we ask whether harmonised national measures of rental prevalence contain information about subsequent agricultural total factor productivity growth.\n\n",
  "We link FAO agricultural-census indicators for 65 countries with USDA Economic Research Service agricultural TFP data and World Bank development indicators. A 10-percentage-point higher rental-area share is associated with a -0.0011 change in annualised five-year post-census TFP growth (HC3 p = 0.598), and this interpretation remains stable across predefined robustness checks. The design is explicitly associational and all quantitative results are generated through a reproducible R-based workflow.\n\n",
  "The contribution is not another estimate of whether land rental can improve productivity within a particular country. Instead, the paper provides a cross-country boundary test of whether the mechanism documented in the microeconomic literature survives aggregation into a readily observable national indicator. To our knowledge, the existing literature has not established whether harmonised cross-country census measures of rental prevalence provide a systematic signal of subsequent national agricultural productivity dynamics. Our results show that rental-market prevalence and rental-market effectiveness should not be treated as interchangeable concepts.\n\n",
  "We believe this distinction is relevant to the readership of the *Journal of Agricultural Economics* because it bears directly on the interpretation of internationally comparable land-market indicators, agricultural resource allocation and the design of evidence for land-market policy. Transaction prevalence is substantially easier to observe across countries than the institutional frictions that determine the quality and direction of land reallocation; our findings show the limits of using the former as a proxy for the latter.\n\n",
  "The manuscript is original, is not under consideration elsewhere, and all authors have reviewed and approved the submitted version. Source data are publicly available from FAO, USDA ERS and the World Bank. All quantitative results are generated from documented source data and R code, and reproducibility materials can be provided in accordance with the journal's requirements.\n\n",
  "OpenAI ChatGPT was used to support literature-search planning, manuscript organisation, language editing and reproducibility-workflow planning and review. All bibliographic records and factual claims were checked against scholarly or official sources; all quantitative analyses were executed from source data in R; all analytical code was reviewed by the authors; and the authors take full responsibility for the manuscript.\n\n",
  "Thank you for considering our work.\n\n",
  "Sincerely,\n\n",
  "José Vicente Ordóñez Yaguache  \n",
  "Andrea Yolanda Jaramillo Castillo  \n",
  "Universidad Nacional de Loja, Ecuador\n\n",
  "Corresponding author: José Vicente Ordóñez Yaguache  \n",
  "Email: jose.ordonez@unl.edu.ec\n"
)

writeLines(strsplit(cover_text, "\n", fixed = TRUE)[[1]], cover, useBytes = TRUE)

# ------------------------------------------------------------------
# 7. QUARTO CONFIG
# ------------------------------------------------------------------

q <- readLines(quarto, warn = FALSE, encoding = "UTF-8")
q <- sub(
  "article_erae_anonymous.qmd",
  "article_jae_anonymous.qmd",
  q,
  fixed = TRUE
)
writeLines(q, quarto, useBytes = TRUE)

cat("JAE_ADAPTATION_OK\n")
cat("Updated manuscript:", article, "\n")
cat("Updated cover letter:", cover, "\n")
cat("Updated Quarto config:", quarto, "\n")
# ------------------------------------------------------------------
# 8. PRECISION OF THE NOVELTY / PROXY CLAIM
# ------------------------------------------------------------------

article <- file.path(
  "submission",
  "journal-agricultural-economics",
  "article_jae_anonymous.qmd"
)

cover <- file.path(
  "submission",
  "journal-agricultural-economics",
  "cover-letter.md"
)

txt <- paste(
  readLines(article, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

old1 <- "The evidence therefore distinguishes rental-market activity from rental-market effectiveness: countries with more rental activity do not systematically exhibit faster subsequent productivity growth."

new1 <- "The evidence therefore suggests that rental-market activity should not be treated as a direct proxy for rental-market effectiveness: countries with more rental activity do not systematically exhibit faster subsequent productivity growth."

old2 <- "Second, it distinguishes rental-market activity from rental-market effectiveness: a larger share of rented agricultural area need not imply that land moves toward higher-productivity operators or that contractual institutions support productivity-enhancing investment."

new2 <- "Second, it evaluates whether rental-market activity can serve as an aggregate proxy for rental-market effectiveness: a larger share of rented agricultural area need not imply that land moves toward higher-productivity operators or that contractual institutions support productivity-enhancing investment."

if (!grepl(old1, txt, fixed = TRUE)) {
  stop("Abstract claim not found.")
}

if (!grepl(old2, txt, fixed = TRUE)) {
  stop("Introduction claim not found.")
}

txt <- sub(old1, new1, txt, fixed = TRUE)
txt <- sub(old2, new2, txt, fixed = TRUE)

writeLines(
  strsplit(txt, "\n", fixed = TRUE)[[1]],
  article,
  useBytes = TRUE
)

cover_txt <- paste(
  readLines(cover, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

old3 <- "Our results show that rental-market prevalence and rental-market effectiveness should not be treated as interchangeable concepts."

new3 <- "Our results suggest that rental-market prevalence should not be treated as an interchangeable proxy for rental-market effectiveness."

if (!grepl(old3, cover_txt, fixed = TRUE)) {
  stop("Cover-letter claim not found.")
}

cover_txt <- sub(old3, new3, cover_txt, fixed = TRUE)

writeLines(
  strsplit(cover_txt, "\n", fixed = TRUE)[[1]],
  cover,
  useBytes = TRUE
)

cat("JAE_CLAIM_PRECISION_OK\n")
