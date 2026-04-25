#' Write Data to Multiple File Formats
#'
#' Writes a data frame to CSV, XLSX, Parquet, RDS, SAS transport (XPT), or
#' Google Sheets. The target format is inferred from the file extension or, for
#' Google Sheets, from the presence of `"google"` in the path string.
#'
#' @param data A data frame to write.
#' @param path A string specifying the output file path or Google Sheets URL/ID.
#' @param sheet A string specifying the sheet name for XLSX or Google Sheets.
#'   Default `"Sheet1"`.
#' @param ... Additional arguments passed to the underlying writer function.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of writing data.
#' @export
#'
#' @examples
#' \dontrun{
#' write_data(mtcars, "output.csv")
#' write_data(mtcars, "output.xlsx", sheet = "Cars")
#' write_data(mtcars, "output.parquet")
#' write_data(mtcars, "output.rds")
#' }
write_data <- function(data, path, sheet = "Sheet1", ...) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(path)
  checkmate::assert_string(sheet)

  #--- Determine file format ---
  file_ext  <- tolower(tools::file_ext(path))
  is_gsheet <- grepl("google", tolower(path), fixed = TRUE)

  file_type <- if (is_gsheet) {
    "gsheet"
  } else if (file_ext %in% c("csv", "xlsx", "rds", "parquet", "xpt")) {
    file_ext
  } else {
    cli::cli_abort(
      "Unsupported file format: {.file {path}}.
       Supported formats: CSV, XLSX, Parquet, RDS, XPT, Google Sheets."
    )
  }

  #--- Write to target format ---
  switch(
    file_type,
    "csv"     = data.table::fwrite(data, path, ...),
    "xlsx"    = openxlsx2::write_xlsx(data, path, sheet = sheet, ...),
    "parquet" = arrow::write_parquet(x = data, sink = path, ...),
    "xpt"     = haven::write_xpt(data = data, path = path),
    "rds"     = base::saveRDS(object = data, file = path, ...),
    "gsheet"  = googlesheets4::write_sheet(data = data, ss = path, sheet = sheet)
  )

  cli::cli_alert_success("Data written to {.file {path}}.")
  invisible(NULL)
}
