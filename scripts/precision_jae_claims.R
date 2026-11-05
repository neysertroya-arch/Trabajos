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

# ------------------------------------------------------------
# MANUSCRIPT
# ------------------------------------------------------------

txt <- paste(
  readLines(article, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

old1 <- "The evidence therefore distinguishes rental-market activity from rental-market effectiveness: countries with more rental activity do not systematically exhibit faster subsequent productivity growth."

new1 <- "The evidence therefore suggests that rental-market activity should not be treated as a direct proxy for rental-market effectiveness: countries with more rental activity do not systematically exhibit faster subsequent productivity growth."

old2 <- "Second, it distinguishes rental-market activity from rental-market effectiveness: a larger share of rented agricultural area need not imply that land moves toward higher-productivity operators or that contractual institutions support productivity-enhancing investment."

new2 <- "Second, it evaluates whether rental-market activity can serve as an aggregate proxy for rental-market effectiveness: a larger share of rented agricultural area need not imply that land moves toward higher-productivity operators or that contractual institutions support productivity-enhancing investment."

if (grepl(old1, txt, fixed = TRUE)) {
  txt <- sub(old1, new1, txt, fixed = TRUE)
} else if (!grepl(new1, txt, fixed = TRUE)) {
  stop("Neither old nor new abstract claim found.")
}

if (grepl(old2, txt, fixed = TRUE)) {
  txt <- sub(old2, new2, txt, fixed = TRUE)
} else if (!grepl(new2, txt, fixed = TRUE)) {
  stop("Neither old nor new introduction claim found.")
}

writeLines(
  strsplit(txt, "\n", fixed = TRUE)[[1]],
  article,
  useBytes = TRUE
)

# ------------------------------------------------------------
# COVER LETTER
# ------------------------------------------------------------

cover_txt <- paste(
  readLines(cover, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

old3 <- "Our results show that rental-market prevalence and rental-market effectiveness should not be treated as interchangeable concepts."

new3 <- "Our results suggest that rental-market prevalence should not be treated as an interchangeable proxy for rental-market effectiveness."

if (grepl(old3, cover_txt, fixed = TRUE)) {
  cover_txt <- sub(old3, new3, cover_txt, fixed = TRUE)
} else if (!grepl(new3, cover_txt, fixed = TRUE)) {
  stop("Neither old nor new cover-letter claim found.")
}

writeLines(
  strsplit(cover_txt, "\n", fixed = TRUE)[[1]],
  cover,
  useBytes = TRUE
)

cat("JAE_CLAIM_PRECISION_OK\n")
