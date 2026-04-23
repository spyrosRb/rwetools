#' Interactive bar plot (Highcharts)
#'
#' Creates an interactive bar plot using the highcharter package.
#'
#' @param data A data frame.
#' @param x Column name (unquoted) for the categorical axis.
#' @param y Column name (unquoted) for the numeric axis.
#' @param group Column name (unquoted) for the grouping variable. Optional.
#' @param stacking Stacking mode: `"normal"`, `"percent"`, or NULL (grouped). Default NULL.
#' @param title Plot title. Default NULL.
#' @param subtitle Plot subtitle. Default NULL.
#' @param x_label X-axis label. Defaults to the column name.
#' @param y_label Y-axis label. Defaults to the column name.
#' @param palette Character vector of colours. Default uses Highcharts defaults.
#' @param flip Logical. If TRUE, renders a horizontal bar chart. Default FALSE.
#'
#' @return A `highchart` object.
#' @export
plot_hc_bar <- function(data,
                        x,
                        y,
                        group = NULL,
                        stacking = NULL,
                        title = NULL,
                        subtitle = NULL,
                        x_label = NULL,
                        y_label = NULL,
                        palette = NULL,
                        flip = FALSE) {

  x_var   <- rlang::ensym(x)
  y_var   <- rlang::ensym(y)
  grp_var <- if (!missing(group)) rlang::ensym(group) else NULL

  x_lab <- x_label %||% rlang::as_label(x_var)
  y_lab <- y_label %||% rlang::as_label(y_var)

  chart_type <- if (flip) "bar" else "column"

  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(type = chart_type) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_xAxis(
      categories = unique(dplyr::pull(data, !!x_var)),
      title = list(text = x_lab)
    ) %>%
    highcharter::hc_yAxis(title = list(text = y_lab)) %>%
    highcharter::hc_plotOptions(
      series = list(stacking = stacking)
    )

  if (!is.null(grp_var)) {
    series_data <- data %>%
      dplyr::group_by(!!grp_var) %>%
      dplyr::group_map(~ list(
        name = as.character(.y[[1]]),
        data = .x %>% dplyr::pull(!!y_var)
      ))

    for (s in series_data) {
      hc <- hc %>% highcharter::hc_add_series(name = s$name, data = s$data)
    }
  } else {
    hc <- hc %>% highcharter::hc_add_series(
      name = y_lab,
      data = dplyr::pull(data, !!y_var)
    )
  }

  if (!is.null(palette)) hc <- hc %>% highcharter::hc_colors(palette)

  hc
}
