#' Interactive USA Map (Highcharts)
#'
#' Generates an interactive Highcharts choropleth map for a US-level dataset.
#' Use `glimpse_data = TRUE` to inspect the map data and identify the correct
#' join column names before plotting.
#'
#' @param data A data frame containing the values to map.
#' @param values A string specifying the column in `data` with numeric values
#'   to display.
#' @param by_x A string specifying the column name in the Highcharts map data
#'   to join on (e.g. `"hc-key"`).
#' @param by_y A string specifying the column name in `data` to join on.
#' @param map A string specifying the Highcharts map path. Default
#'   `"countries/us/us-all"`. See
#'   \url{https://code.highcharts.com/mapdata/} for available maps.
#' @param decimals Integer number of decimal places for tooltip values.
#'   Default `0`.
#' @param glimpse_data Logical. If TRUE, downloads and returns the map data
#'   for inspection without rendering the plot. Default `FALSE`.
#' @param title Optional string for the map title. Default `NULL`.
#'
#' @return A `highchart` map object, or (when `glimpse_data = TRUE`)
#'   invisibly returns the raw map data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   code     = c("us-ca", "us-tx", "us-ny", "us-fl"),
#'   patients = c(120, 95, 80, 60)
#' )
#'
#' # Inspect map join columns first
#' plot_usa_map(df, values = "patients", by_x = "hc-key", by_y = "code",
#'              glimpse_data = TRUE)
#'
#' # Render map
#' plot_usa_map(df, values = "patients", by_x = "hc-key", by_y = "code",
#'              title = "Patients per State")
#' }
plot_usa_map <- function(data,
                         values,
                         by_x,
                         by_y,
                         map          = "countries/us/us-all",
                         decimals     = 0,
                         glimpse_data = FALSE,
                         title        = NULL) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(values)
  checkmate::assert_string(by_x)
  checkmate::assert_string(by_y)
  checkmate::assert_string(map)
  checkmate::assert_integerish(decimals, lower = 0)
  checkmate::assert_logical(glimpse_data, len = 1)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_names(names(data), must.include = c(values, by_y))

  #--- Download map data ---
  cli::cli_alert_info("Downloading map data from {.url https://code.highcharts.com/mapdata/}.")
  mapdata <- highcharter::get_data_from_map(highcharter::download_map_data(map))

  #--- Glimpse mode ---
  if (glimpse_data) {
    cli::cli_alert_info("Glimpse mode enabled — displaying map data to identify join columns.")
    dplyr::glimpse(mapdata)
    return(invisible(mapdata))
  }

  #--- Render map ---
  highcharter::hcmap(
    map,
    data        = data,
    value       = values,
    joinBy      = c(by_x, by_y),
    dataLabels  = list(enabled = TRUE, format = "{point.name}"),
    borderColor = "#FAFAFA",
    borderWidth = 0.1,
    tooltip     = list(
      pointFormat  = "{point.name}: {point.value}",
      valueDecimals = decimals
    )
  ) |>
    highcharter::hc_title(text = title)
}
