#' Interactive Treemap (Highcharts)
#'
#' Generates an interactive treemap chart using `highcharter`, where tile size
#' and colour both reflect the count of records per group.
#'
#' @param data A data frame containing the data to be summarised and plotted.
#' @param group_by A string specifying the column to group by (defines tile
#'   labels).
#' @param decimals Integer number of decimal places shown in the tooltip
#'   percentage. Default `2`.
#'
#' @return A `highchart` object.
#' @export
#'
#' @examples
#' \dontrun{
#' data("mpg", package = "ggplot2")
#' plot_hc_treemap(data = mpg, group_by = "manufacturer")
#' }
plot_hc_treemap <- function(data, group_by, decimals = 2) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(group_by)
  checkmate::assert_names(names(data), must.include = group_by)
  checkmate::assert_integerish(decimals, lower = 0)

  #--- Summarise counts and percentages ---
  summary_data <- data |>
    dplyr::group_by(.data[[group_by]]) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(pct = round(n / sum(n) * 100, decimals)) |>
    dplyr::arrange(dplyr::desc(n))

  #--- Render treemap ---
  highcharter::hchart(
    summary_data,
    "treemap",
    highcharter::hcaes(
      x     = .data[[group_by]],
      value = n,
      color = n
    )
  ) |>
    highcharter::hc_tooltip(
      pointFormat = paste0(
        "{point.name}: <b>{point.value}</b><br>",
        "Percentage: <b>{point.pct:.", decimals, "f}%</b>"
      )
    )
}
