# Focused editorial audit for Journal of Rural Studies transfer.
# Runs before analytical reconstruction to fail fast on manuscript/package issues.

article_file <- file.path("manuscript", "article.qmd")
bib_file <- file.path("manuscript", "references.bib")
highlights_file <- file.path("submission", "journal-of-rural-studies", "highlights.md")

for (f in c(article_file, bib_file, highlights_file)) {
  if (!file.exists(f)) stop("JRS_AUDIT_FAIL: missing file: ", f)
}

article <- readLines(article_file, warn = FALSE, encoding = "UTF-8")
bib <- readLines(bib_file, warn = FALSE, encoding = "UTF-8")
highlights <- readLines(highlights_file, warn = FALSE, encoding = "UTF-8")
article_text <- paste(article, collapse = "\n")

expected_headings <- c(
  "# 1. Introduction",
  "# 2. Conceptual framework",
  "# 3. Data and empirical strategy",
  "# 4. Results",
  "# 5. Discussion and implications for rural policy",
  "# 6. Conclusion",
  "# References"
)
actual_h1 <- article[grepl("^# ", article)]
if (!identical(actual_h1, expected_headings)) {
  stop("JRS_AUDIT_FAIL: unexpected H1 hierarchy: ", paste(actual_h1, collapse = " | "))
}

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
if (length(citation_keys) < 50L) stop("JRS_AUDIT_FAIL: fewer than 50 cited references")
if (length(missing_in_bib)) stop("JRS_AUDIT_FAIL: missing BibTeX keys: ", paste(missing_in_bib, collapse=", "))
if (length(uncited_in_bib)) stop("JRS_AUDIT_FAIL: uncited BibTeX entries: ", paste(uncited_in_bib, collapse=", "))

front <- which(trimws(article) == "---")
if (length(front) < 2L) stop("JRS_AUDIT_FAIL: malformed YAML")
yaml <- article[seq_len(front[2])]
abs_start <- which(startsWith(trimws(yaml), "abstract:"))
kw_line <- which(startsWith(trimws(yaml), "keywords:"))
if (length(abs_start) != 1L || length(kw_line) != 1L) stop("JRS_AUDIT_FAIL: abstract or keywords missing")
abs_lines <- yaml[seq.int(abs_start + 1L, kw_line - 1L)]
abs_text <- paste(trimws(abs_lines), collapse = " ")
abs_words <- strsplit(gsub("[^[:alnum:]'-]+", " ", abs_text), " +")[[1]]
abs_words <- abs_words[nzchar(abs_words)]
if (length(abs_words) > 250L) stop("JRS_AUDIT_FAIL: abstract exceeds 250 words")

kw_text <- sub("^keywords:\\s*\\[", "", trimws(yaml[kw_line]))
kw_text <- sub("\\]\\s*$", "", kw_text)
keywords <- trimws(strsplit(kw_text, ",")[[1]])
if (length(keywords) < 1L || length(keywords) > 7L) stop("JRS_AUDIT_FAIL: keyword count outside 1-7")

bullets <- sub("^- ", "", grep("^- ", highlights, value = TRUE))
if (length(bullets) < 3L || length(bullets) > 5L) stop("JRS_AUDIT_FAIL: highlights must contain 3-5 bullets")
lens <- nchar(bullets, type = "chars")
if (any(lens > 85L)) stop("JRS_AUDIT_FAIL: highlight exceeds 85 characters")

spanish_markers <- c("COPIA PRIVADA", "EN ESPAÑOL", "REVISIÓN DE LOS AUTORES")
hits <- spanish_markers[vapply(spanish_markers, function(z) grepl(z, article_text, fixed=TRUE), logical(1))]
if (length(hits)) stop("JRS_AUDIT_FAIL: Spanish marker detected")

cat("JRS_EDITORIAL_AUDIT_PASS\n")
cat("Cited references:", length(citation_keys), "\n")
cat("BibTeX entries:", length(bib_keys), "\n")
cat("Abstract words:", length(abs_words), "\n")
cat("Keywords:", length(keywords), "\n")
cat("Highlight lengths:", paste(lens, collapse=", "), "\n")
