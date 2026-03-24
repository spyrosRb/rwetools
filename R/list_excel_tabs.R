#' List sheet names from an Excel file
#'
#' @param path Character. Path to an Excel (.xlsx) file.
#'
#' @return Character vector of sheet names (lowercase, excluding "info").
#'
#' @noRd
list_excel_tabs <- function(path) {

  # --- Check function arguments ---
  checkmate::assert_file_exists(path)

  # --- Extract sheet names ---
  wb <- openxlsx2::wb_load(path)
  sheet_names <- openxlsx2::wb_get_sheet_names(wb)
  sheet_names <- setdiff(tolower(sheet_names), "info")

  sheet_names
}
