# Project setup -----------------------------------------------------------

options(stringsAsFactors = FALSE)

required_packages <- c(
  "here",
  "dplyr",
  "readr",
  "tidyr",
  "stringr",
  "purrr",
  "janitor",
  "countrycode",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    ". Restore the project environment before running the pipeline."
  )
}

message("Project setup OK")
