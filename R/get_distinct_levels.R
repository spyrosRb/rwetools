#' Get Distinct Values and Frequencies of a Column
#'
#' Returns a summary tibble of the distinct values in a column, with their
#' counts and percentages, sorted by descending frequency.
#'
#' @param df A data frame.
#' @param var A string specifying the column name to summarise.
#'
#' @return A tibble with columns `<var>`, `n`, and `pct`. Returns
#'   `invisible(NULL)` with a warning if `var` is not a column in `df`.
#' @export
#'
#' @examples
#' get_distinct_levels(mtcars, "cyl")
#' get_distinct_levels(iris, "Species")
get_distinct_levels <- function(df, var) {

  #--- Argument checks ---
  checkmate::assert_data_frame(df)
  checkmate::assert_string(var, min.chars = 1)

  #--- Verify column exists ---
  if (!var %in% names(df)) {
    cli::cli_alert_warning("Column {.val {var}} does not exist in the data frame.")
    return(invisible(NULL))
  }

  #--- Compute count and percentage per distinct level ---
  total <- nrow(df)

  df |>
    dplyr::count(.data[[var]], name = "n") |>
    dplyr::mutate(pct = paste0(round(n / total * 100, 2), "%")) |>
    dplyr::arrange(dplyr::desc(n))
}
