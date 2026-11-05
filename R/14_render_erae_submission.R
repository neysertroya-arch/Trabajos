# 14_render_erae_submission.R ----------------------------------------------
# One-command ERAE build + single render + anonymity check.
# No inferential model is re-estimated here.

source("R/13_build_erae_submission.R")

input <- "submission/european-review-agricultural-economics/article_erae_anonymous.qmd"
output <- "submission/european-review-agricultural-economics/article_erae_anonymous.docx"

run_quarto <- function() {
  if (requireNamespace("quarto", quietly = TRUE)) {
    quarto::quarto_render(input = input, output_format = "docx", quiet = TRUE)
    return(invisible(TRUE))
  }

  q <- Sys.which("quarto")
  if (!nzchar(q)) {
    candidates <- c(
      "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
      "C:/Program Files/RStudio/bin/quarto/bin/quarto.exe"
    )
    q <- candidates[file.exists(candidates)][1]
  }
  if (is.na(q) || !nzchar(q) || !file.exists(q)) {
    stop("Quarto executable not found. Open the project in RStudio and install/load the 'quarto' R package, or verify the RStudio Quarto path.")
  }

  status <- system2(q, c("render", shQuote(input), "--to", "docx"))
  if (!identical(status, 0L)) stop("Quarto render failed with status ", status)
}

run_quarto()

if (!file.exists(output)) stop("Expected ERAE DOCX was not created: ", output)
if (file.info(output)$size < 10000) stop("Unexpectedly small ERAE DOCX: ", file.info(output)$size, " bytes")

# Apply ERAE presentation requirements and verify the patch structurally.
python_bin <- Sys.which("python")
if (!nzchar(python_bin)) stop("Python executable not found for ERAE DOCX presentation patch.")
patch_script <- "scripts/patch_erae_docx.py"
if (!file.exists(patch_script)) stop("Missing ERAE DOCX patch helper: ", patch_script)
patch_status <- system2(python_bin, c(shQuote(patch_script), shQuote(output)))
if (!identical(patch_status, 0L)) stop("ERAE DOCX presentation patch failed with status ", patch_status)

tmp <- tempfile("erae_docx_")
dir.create(tmp)
utils::unzip(output, exdir = tmp)
xml_files <- list.files(tmp, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)
xml_text <- paste(unlist(lapply(xml_files, readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n")

identifiers <- c(
  "José Vicente Ordóñez Yaguache",
  "Andrea Yolanda Jaramillo Castillo",
  "Universidad Nacional de Loja",
  "josevicente1980",
  "0000-0002-3349-6714",
  "0009-0003-5584-9845",
  "jose.ordonez@unl.edu.ec"
)
hits <- identifiers[vapply(identifiers, function(z) grepl(z, xml_text, fixed = TRUE), logical(1))]
if (length(hits)) stop("DOCX_ANONYMITY_AUDIT_FAIL: ", paste(hits, collapse = "; "))

cat("ERAE_RENDER_OK\n")
cat("DOCX: ", normalizePath(output, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat("Size: ", file.info(output)$size, " bytes\n", sep = "")
cat("DOCX_ANONYMITY_AUDIT_PASS\n")
