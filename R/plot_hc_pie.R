#' Interactive pie chart (Highcharts)
#'
#' Creates an interactive pie chart using the highcharter package.
#'
#' @param data A data frame.
#' @param category Column name (unquoted) for the slice categories.
#' @param value Column name (unquoted) for the numeric slice values.
#' @param title Plot title. Default NULL.
#' @param subtitle Plot subtitle. Default NULL.
#' @param palette Character vector of colours. Default uses Highcharts defaults.
#' @param donut Logical. If TRUE, renders a donut chart. Default FALSE.
#' @param labels Logical. If TRUE, shows data labels on slices. Default TRUE.
#'
#' @return A `highchart` object.
#' @export
plot_hc_pie <- function(data,
                        category,
                        value,
                        title = NULL,
                        subtitle = NULL,
                        palette = NULL,
                        donut = FALSE,
                        labels = TRUE) {

  cat_var <- rlang::ensym(category)
  val_var <- rlang::ensym(value)

  series_data <- data %>%
    dplyr::transmute(
      name = as.character(!!cat_var),
      y    = as.numeric(!!val_var)
    ) %>%
    highcharter::list_parse()

  inner_size <- if (donut) "50%" else "0%"

  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "pie") %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_add_series(
      name      = "Value",
      data      = series_data,
      innerSize = inner_size
    ) %>%
    highcharter::hc_plotOptions(
      pie = list(
        dataLabels = list(enabled = labels)
      )
    )

  if (!is.null(palette)) hc <- hc %>% highcharter::hc_colors(palette)

  hc
}
