#' Detect file type from a path or URL
#'
#' Returns a file type string based on the file extension or URL pattern.
#' Supported types: `"csv"`, `"xlsx"`, `"rds"`, `"parquet"`, `"sas7bdat"`,
#' `"xpt"`, `"gsheet"`.
#'
#' @param path Character. File path or URL.
#'
#' @return Character string indicating the file type.
#'
#' @noRd
extract_file_type <- function(path) {

  # --- Check function arguments ---
  checkmate::assert_string(path, null.ok = FALSE)

  # --- Detect file type ---
  path <- tolower(path)

  if (stringr::str_detect(path, "google")) {
    return("gsheet")
  }

  ext <- tools::file_ext(path)

  ext_map <- c(
    csv      = "csv",
    xlsx     = "xlsx",
    rds      = "rds",
    parquet  = "parquet",
    sas7bdat = "sas7bdat",
    xpt      = "xpt"
  )

  file_type <- ext_map[ext]

  if (is.na(file_type)) {
    cli::cli_warn(c(
      "Unsupported file type detected.",
      "x" = "Supported file types are: csv, xlsx, rds, parquet, sas7bdat, xpt."
    ))
  }

  file_type
}
