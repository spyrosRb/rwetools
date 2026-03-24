#' List sheet names from a Google Sheet
#'
#' @param path Character. URL or ID of the Google Sheet.
#'
#' @return Character vector of sheet names (lowercase, excluding "info").
#'
#' @noRd
list_gsheets_tabs <- function(path) {

  # --- Check function arguments ---
  checkmate::assert_string(path)

  # --- Authenticate with Google Sheets ---
  cli::cli_alert_info("Authenticating with Google Sheets...")
  googlesheets4::gs4_auth()

  # --- Extract sheet names ---
  sheet_info <- googlesheets4::gs4_get(path)
  sheet_names <- sheet_info$sheets$name
  sheet_names <- setdiff(tolower(sheet_names), "info")

  sheet_names
}
