#' Render Mean Change Summary Table
#'
#' Generates a styled HTML table using `kableExtra` to display the output of
#' [summarize_mean_change()]. Handles both raw-only and raw + adjusted outputs
#' automatically based on the columns present in `data`.
#'
#' @param data A data frame returned by [summarize_mean_change()], containing
#'   at minimum: visit, n, raw_mean, raw_ci_lower, raw_ci_upper. Adjusted
#'   columns (adjusted_mean, adj_ci_lower, adj_ci_upper) are included
#'   automatically when present.
#' @param fixed_header Logical. If TRUE, the header remains fixed on scroll.
#'   Default `TRUE`.
#' @param bootstrap_options A character vector of Bootstrap styling options.
#'   Default `c("striped", "hover", "condensed")`.
#' @param full_width Logical. If TRUE, the table spans the full page width.
#'   Default `TRUE`.
#' @param escape_html Logical. If TRUE, escapes HTML in cell content.
#'   Default `FALSE`.
#'
#' @return A styled `kableExtra` HTML table.
#' @seealso [summarize_mean_change()]
#' @export
#'
#' @examples
#' \dontrun{
#' results <- summarize_mean_change(
#'   data         = df,
#'   patient_id   = "patient_id",
#'   visit        = "visit",
#'   score_change = "weight_change"
#' )
#'
#' render_mean_change_table(results)
#' }
render_mean_change_table <- function(
    data,
    fixed_header      = TRUE,
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = TRUE,
    escape_html       = FALSE
) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_logical(fixed_header, len = 1)
  checkmate::assert_character(bootstrap_options, min.len = 1)
  checkmate::assert_logical(full_width, len = 1)
  checkmate::assert_logical(escape_html, len = 1)
  checkmate::assert_names(names(data), must.include = c("n", "raw_mean", "raw_ci_lower", "raw_ci_upper"))

  #--- Detect whether adjusted columns are present ---
  has_adjusted <- "adjusted_mean" %in% names(data)

  if (has_adjusted) {
    col_names    <- c("Visit", "N", "Raw Mean", "Raw CI Lower", "Raw CI Upper",
                      "Adjusted Mean", "Adj. CI Lower", "Adj. CI Upper")
    header_spans <- c(" " = 2, "Raw" = 3, "Adjusted" = 3)
  } else {
    col_names    <- c("Visit", "N", "Raw Mean", "Raw CI Lower", "Raw CI Upper")
    header_spans <- c(" " = 2, "Raw" = 3)
  }

  #--- Render styled table ---
  data |>
    kableExtra::kable(
      format    = "html",
      row.names = FALSE,
      escape    = escape_html,
      col.names = col_names
    ) |>
    kableExtra::kable_styling(
      fixed_thead       = fixed_header,
      bootstrap_options = bootstrap_options,
      full_width        = full_width,
      position          = "center"
    ) |>
    kableExtra::add_header_above(
      header_spans,
      escape     = FALSE
    )
}
