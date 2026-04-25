#' Summarise a Numeric Variable
#'
#' Computes descriptive statistics (n, missing, mean, SD, min, Q25, median,
#' Q75, max) for a numeric column. Works on plain and grouped data frames —
#' pass a grouped data frame to get per-group summaries.
#'
#' @param data A data frame (optionally grouped via [dplyr::group_by()]).
#' @param value A string specifying the numeric column to summarise.
#' @param na.rm Logical. If `TRUE` (default), `NA` values are excluded from
#'   all statistics.
#'
#' @return A tibble with one row per group (or one row overall) and columns
#'   `n`, `missing`, `mean`, `sd`, `min`, `q25`, `median`, `q75`, `max`.
#' @export
#'
#' @examples
#' summarise_vars(mtcars, "mpg")
#' mtcars |> dplyr::group_by(cyl) |> summarise_vars("mpg")
summarise_vars <- function(data, value, na.rm = TRUE) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(value, min.chars = 1)
  checkmate::assert_logical(na.rm, len = 1)
  checkmate::assert_names(names(data), must.include = value)

  #--- Compute summary statistics ---
  data |>
    dplyr::summarise(
      n       = sum(!is.na(.data[[value]])),
      missing = sum(is.na(.data[[value]])),
      mean    = round(mean(.data[[value]],               na.rm = na.rm), 2),
      sd      = round(sd(.data[[value]],                 na.rm = na.rm), 3),
      min     = round(min(.data[[value]],                na.rm = na.rm), 2),
      q25     = round(quantile(.data[[value]], 0.25,     na.rm = na.rm), 2),
      median  = round(median(.data[[value]],             na.rm = na.rm), 2),
      q75     = round(quantile(.data[[value]], 0.75,     na.rm = na.rm), 2),
      max     = round(max(.data[[value]],                na.rm = na.rm), 2)
    )
}
