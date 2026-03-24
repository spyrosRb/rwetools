#' Read data from various file formats
#'
#' Dispatches to the appropriate reader based on file type detected by
#' `extract_file_type()`. Supports CSV, Excel, Parquet, SAS, RDS, and
#' Google Sheets.
#'
#' @param path Character. Path to the data file or Google Sheet URL.
#' @param sheet Character or NULL. Sheet name for Excel or Google Sheet files.
#'
#' @return A data frame with cleaned column names.
#'
#' @examples
#' \dontrun{
#' read_data("data.csv")
#' read_data("data.xlsx", sheet = "Sheet1")
#' read_data("https://docs.google.com/spreadsheets/d/abc123", sheet = "bmi")
#' }
#'
#' @noRd
read_data <- function(path, sheet = NULL) {

  # --- Check function arguments ---
  checkmate::assert_character(path, len = 1, null.ok = FALSE)
  checkmate::assert(
    checkmate::check_character(sheet, len = 1, null.ok = FALSE),
    checkmate::check_numeric(sheet, len = 1, null.ok = FALSE)
  )

  # --- Detect file type ---
  file_type <- extract_file_type(path)

  # --- Validate sheet parameter for formats that require it ---
  if (file_type %in% c("gsheet", "xlsx") && is.null(sheet)) {
    cli::cli_abort("Please specify a sheet to read from for {.val {file_type}} files.")
  }

  if (is.na(file_type)) {
    cli::cli_abort("Unsupported file format: {.file {path}}")
  }

  # --- Read data based on file type ---
  data <- switch(
    file_type,

    "csv" = data.table::fread(path) %>%
      janitor::clean_names(),

    "xlsx" = openxlsx2::read_xlsx(path, sheet = sheet) %>%
      janitor::clean_names(),

    "parquet" = arrow::read_parquet(path) %>%
      janitor::clean_names(),

    "sas7bdat" = haven::read_sas(path) %>%
      janitor::clean_names(),

    "xpt" = haven::read_xpt(path) %>%
      janitor::clean_names(),

    "rds" = base::readRDS(path) %>%
      janitor::clean_names(),

    "gsheet" = googlesheets4::with_gs4_quiet(
      googlesheets4::read_sheet(path, sheet = sheet)
    ) %>%
      janitor::clean_names()
  )

  cli::cli_alert_info("{nrow(data)} rows x {ncol(data)} columns loaded.")

  data
}
