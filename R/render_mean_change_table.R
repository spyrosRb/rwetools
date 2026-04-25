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
#' @param header_color Background colour for header cells. Default `"#1482FA"`.
#' @param header_text_color Text colour for header cells. Default `"#FFFFFF"`.
#' @param header_font_size Font size for header text in points. Default `14`.
#' @param body_font_size Font size for body text in points. Default `12`.
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
    header_color      = "#1482FA",
    header_text_color = "#FFFFFF",
    header_font_size  = 14,
    body_font_size    = 12,
    fixed_header      = TRUE,
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = TRUE,
    escape_html       = FALSE
) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(header_color)
  checkmate::assert_string(header_text_color)
  checkmate::assert_number(header_font_size, lower = 1)
  checkmate::assert_number(body_font_size, lower = 1)
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
      font_size         = body_font_size,
      fixed_thead       = fixed_header,
      bootstrap_options = bootstrap_options,
      full_width        = full_width,
      position          = "center"
    ) |>
    kableExtra::add_header_above(
      header_spans,
      escape     = FALSE,
      background = header_color,
      color      = header_text_color,
      font_size  = header_font_size
    ) |>
    kableExtra::row_spec(
      0,
      bold       = TRUE,
      font_size  = header_font_size,
      background = header_color,
      color      = header_text_color
    )
}
