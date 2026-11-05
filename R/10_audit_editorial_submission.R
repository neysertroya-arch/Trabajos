# 10_audit_editorial_submission.R -----------------------------------------
# Reproducible editorial audit for the Land Use Policy manuscript.
# This script does not alter scientific results or bibliography content.

article_file <- file.path("manuscript", "article.qmd")
bib_file <- file.path("manuscript", "references.bib")
results_file <- file.path("outputs", "publication", "results_generated.md")

for (f in c(article_file, bib_file, results_file)) {
  if (!file.exists(f)) stop("EDITORIAL_AUDIT_FAIL: missing file: ", f)
}

article <- readLines(article_file, warn = FALSE, encoding = "UTF-8")
bib <- readLines(bib_file, warn = FALSE, encoding = "UTF-8")
results <- readLines(results_file, warn = FALSE, encoding = "UTF-8")
article_text <- paste(article, collapse = "\n")
bib_text <- paste(bib, collapse = "\n")

# 1. Exact core section structure requested for the journal-facing manuscript.
expected_headings <- c(
  "# 1. Introduction",
  "# 2. Conceptual framework",
  "# 3. Data and empirical strategy",
  "# 4. Results",
  "# 5. Discussion and policy implications",
  "# 6. Conclusion",
  "# References"
)
actual_h1 <- article[grepl("^# ", article)]
if (!identical(actual_h1, expected_headings)) {
  stop(
    "EDITORIAL_AUDIT_FAIL: unexpected H1 hierarchy. Found: ",
    paste(actual_h1, collapse = " | ")
  )
}

# 2. Results must be continuous prose: generated fragment cannot emit subheadings.
if (any(grepl("^##+ ", results))) {
  stop("EDITORIAL_AUDIT_FAIL: Results generator contains internal subheadings")
}

# 3. Citation/BibTeX consistency. Every active BibTeX entry must be cited and vice versa.
citation_keys <- unique(unlist(regmatches(
  article_text,
  gregexpr("@[A-Za-z0-9_:-]+", article_text, perl = TRUE)
)))
citation_keys <- sub("^@", "", citation_keys)

bib_keys <- unique(sub(
  "^@[A-Za-z]+\\{([^,]+),.*$",
  "\\1",
  grep("^@[A-Za-z]+\\{[^,]+,", bib, value = TRUE)
))

missing_in_bib <- setdiff(citation_keys, bib_keys)
uncited_in_bib <- setdiff(bib_keys, citation_keys)

if (length(citation_keys) < 50L) {
  stop("EDITORIAL_AUDIT_FAIL: fewer than 50 unique cited references: ", length(citation_keys))
}
if (length(bib_keys) < 50L) {
  stop("EDITORIAL_AUDIT_FAIL: fewer than 50 BibTeX entries: ", length(bib_keys))
}
if (length(missing_in_bib)) {
  stop("EDITORIAL_AUDIT_FAIL: citation keys missing from BibTeX: ", paste(missing_in_bib, collapse = ", "))
}
if (length(uncited_in_bib)) {
  stop("EDITORIAL_AUDIT_FAIL: uncited BibTeX entries: ", paste(uncited_in_bib, collapse = ", "))
}

# 4. Guard against accidental Spanish carry-over from the private reading copy.
spanish_markers <- c(
  "COPIA PRIVADA",
  "EN ESPAÑOL",
  "REVISIÓN DE LOS AUTORES",
  "prevalencia del arrendamiento",
  "crecimiento de la PTF",
  "Disponibilidad de datos",
  "Declaración sobre el uso"
)
hits <- spanish_markers[vapply(
  spanish_markers,
  function(z) grepl(z, article_text, fixed = TRUE),
  logical(1)
)]
if (length(hits)) {
  stop("EDITORIAL_AUDIT_FAIL: Spanish marker(s) detected: ", paste(hits, collapse = "; "))
}

# 5. Abstract length and keyword count.
front <- which(trimws(article) == "---")
if (length(front) < 2L) stop("EDITORIAL_AUDIT_FAIL: malformed YAML")
yaml <- article[seq_len(front[2])]
abs_start <- which(startsWith(trimws(yaml), "abstract:"))
kw_line <- which(startsWith(trimws(yaml), "keywords:"))
if (length(abs_start) != 1L || length(kw_line) != 1L) {
  stop("EDITORIAL_AUDIT_FAIL: abstract/keywords not found exactly once")
}
abs_lines <- yaml[seq.int(abs_start + 1L, kw_line - 1L)]
abs_text <- paste(trimws(abs_lines), collapse = " ")
abs_words <- strsplit(gsub("[^[:alnum:]'-]+", " ", abs_text), " +")[[1]]
abs_words <- abs_words[nzchar(abs_words)]
if (length(abs_words) > 250L) stop("EDITORIAL_AUDIT_FAIL: abstract exceeds 250 words")

kw_text <- sub("^keywords:\\s*\\[", "", trimws(yaml[kw_line]))
kw_text <- sub("\\]\\s*$", "", kw_text)
keywords <- trimws(strsplit(kw_text, ",")[[1]])
if (length(keywords) < 1L || length(keywords) > 7L) {
  stop("EDITORIAL_AUDIT_FAIL: keyword count outside 1-7")
}

cat("EDITORIAL_AUDIT_PASS\n")
cat("Unique cited references:", length(citation_keys), "\n")
cat("BibTeX entries:", length(bib_keys), "\n")
cat("Abstract words:", length(abs_words), "\n")
cat("Keywords:", length(keywords), "\n")
