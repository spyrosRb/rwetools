#' Load medical code lookup lists from a spreadsheet
#'
#' Each sheet is expected to have columns: `code`, `vocabulary`, and
#' `include_code`. Only rows with `include_code == 1` and non-missing
#' `code` values are retained.
#'
#' @param path Character. Path to an Excel file or URL to a Google Sheet.
#' @param sheets Character vector or NULL. Sheet names to read. If NULL,
#'   all sheets are auto-detected (excluding any "info" sheet).
#' @param verbose Logical. Print progress messages. Default TRUE.
#' @param con Redshift database connection object
#'
#' @examples
#' \dontrun{
#' # Auto-detect all sheets
#' code_lists <- load_medical_code_lists(
#'   path = "inst/extdata/truvetaR_codelists.xlsx",
#'   con = con
#' )
#'
#' # Specific sheets only
#' code_lists <- load_medical_code_lists(
#'   path = codes_path,
#'   sheets = c("bmi", "ht", "wt"),
#'   con = con
#' )
#'
#' # Access individual lookups
#' code_lists$bmi
#' code_lists$ht
#' }
#'
#' @export
load_medical_code_lists <- function(path,
                                    sheets = NULL,
                                    verbose = TRUE,
                                    con) {

  # --- Check function arguments ---
  checkmate::assert_string(path)
  checkmate::assert_character(sheets, min.len = 1, null.ok = TRUE)
  checkmate::assert_flag(verbose)
  # checkmate::assert_class(con, "RedshiftConnection")

  # --- Detect available sheets based on file type ---
  file_type <- extract_file_type(path)

  available_sheets <- switch(
    file_type,
    xlsx   = list_excel_tabs(path),
    gsheet = list_gsheets_tabs(path)
  )

  # --- Exclude non-data sheets ---
  available_sheets <- setdiff(tolower(available_sheets), "info")

  if (is.null(sheets)) {
    sheets <- available_sheets

    if (verbose) {
      cli::cli_alert_info("Auto-detected {length(sheets)} sheet{?s}: {.val {sheets}}")
    }
  } else {
    # --- Validate requested sheets exist in the file ---
    missing_sheets <- setdiff(tolower(sheets), available_sheets)

    if (length(missing_sheets) > 0) {
      cli::cli_warn("Sheet{?s} not found in file: {.val {missing_sheets}}")
      sheets <- setdiff(sheets, missing_sheets)
    }

    if (length(sheets) == 0) {
      cli::cli_abort("No valid sheets to process.")
    }

    if (verbose) {
      cli::cli_alert_info("Selected {length(sheets)} sheet{?s} to process: {.val {sheets}}")
    }
  }

  if (verbose) {
    cli::cli_rule()
  }

  # --- Build lookup list for each sheet ---
  results <- stats::setNames(
    vector("list", length(sheets)),
    sheets
  )

  for (sheet in sheets) {

    if (verbose) {
      cli::cli_alert("Processing sheet {.val {sheet}}")
    }

    # --- Read and filter code list data ---
    data <- read_data(path = path, sheet = sheet) %>%
      dplyr::select(code, vocabulary, include_code) %>%
      dplyr::filter(include_code == 1) %>%
      dplyr::mutate(
        code = as.character(stringr::str_replace_all(code, "[`\\.]", "")),
        vocabulary = as.character(tolower(vocabulary))
      ) %>%
      dplyr::filter(!is.na(code))

    # --- Group codes by vocabulary ---
    code_list <- split(data$code, data$vocabulary)

    results[[sheet]] <- lapply(code_list, toupper)

    if (verbose) {
      cli::cli_alert_success("Lookup {.val {sheet}} created successfully.")
      cli::cli_rule()
    }
  }

  invisible(results)
}
