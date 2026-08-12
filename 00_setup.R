## Project setup -----------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "tibble", "readr", "haven", "forcats",
  "broom", "broom.mixed", "lme4", "emmeans", "ggplot2"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running the analysis: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(haven)
  library(forcats)
  library(broom)
  library(broom.mixed)
  library(lme4)
  library(emmeans)
  library(ggplot2)
})

## Raw SOEP data are not distributed with this repository.
## By default, scripts expect the four source files in data_raw/.
## Set SOEP_DATA_DIR to another folder if preferred.
data_dir <- Sys.getenv("SOEP_DATA_DIR", unset = "data_raw")
output_dir <- Sys.getenv("SOEP_OUTPUT_DIR", unset = "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

pl_path <- file.path(data_dir, "pl.csv")
pgen_path <- file.path(data_dir, "pgen.csv")
ppathl_path <- file.path(data_dir, "ppathl.sav")
cognit_path <- file.path(data_dir, "cognit.sav")

source_files <- c(pl_path, pgen_path, ppathl_path, cognit_path)
if (!all(file.exists(source_files))) {
  stop(
    "Required SOEP files were not found. Expected:\n",
    paste(source_files, collapse = "\n"),
    "\nSee README.md for data-access and file-placement instructions."
  )
}

soep_to_na <- function(x) {
  if (inherits(x, "haven_labelled")) x <- as.numeric(x)
  if (is.factor(x)) x <- suppressWarnings(as.numeric(as.character(x)))
  if (is.character(x)) x <- suppressWarnings(as.numeric(x))
  if (is.numeric(x)) {
    x[x %in% c(-1, -2, -3, -4, -5, -7, -8, -9)] <- NA_real_
  }
  x
}

read_soep_csv <- function(path, guess_max = 100000) {
  semicolon <- tryCatch(
    readr::read_delim(
      path, delim = ";", show_col_types = FALSE,
      guess_max = guess_max, progress = FALSE
    ),
    error = function(e) NULL
  )
  comma <- tryCatch(
    readr::read_delim(
      path, delim = ",", show_col_types = FALSE,
      guess_max = guess_max, progress = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(semicolon) && is.null(comma)) stop("Could not read: ", path)
  if (!is.null(semicolon) && is.null(comma)) return(semicolon)
  if (is.null(semicolon) && !is.null(comma)) return(comma)
  if (ncol(semicolon) >= ncol(comma)) semicolon else comma
}

req_cols <- function(df, cols, df_name = "data") {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(df_name, " is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < .001 ~ "< .001",
    TRUE ~ sub("^0", "", sprintf("%.3f", p))
  )
}

sample_summary <- function(df, label) {
  tibble(
    sample = label,
    persons = n_distinct(df$pid),
    person_years = nrow(df),
    first_year = if (nrow(df) > 0) min(df$syear, na.rm = TRUE) else NA_integer_,
    last_year = if (nrow(df) > 0) max(df$syear, na.rm = TRUE) else NA_integer_
  )
}
