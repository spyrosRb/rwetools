#' Interactive pie chart (Highcharts)
#'
#' Creates an interactive pie or donut chart using the highcharter package.
#' Data is prepared internally by renaming columns to the `name`/`y` structure
#' that Highcharts expects natively.
#'
#' @param data A data frame containing at least two columns.
#' @param category A string specifying the column name for slice categories.
#' @param value A string specifying the column name for numeric slice values.
#'   Non-numeric values are coerced; NA rows are dropped silently.
#' @param title A string for the plot title. Default NULL.
#' @param subtitle A string for the plot subtitle. Default NULL.
#' @param colors A character vector of colours, one per category
#'   (e.g. `c("#C40000", "#FF7D29", "#1482FA")`). If NULL, Highcharts default
#'   colours are used.
#' @param donut Logical. If TRUE, renders a donut chart. Default FALSE.
#' @param show_labels Logical. If TRUE (default), displays data labels on
#'   slices showing category name and percentage.
#'
#' @return A `highchart` object.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   category = c("A", "B", "C"),
#'   value    = c(10, 20, 30)
#' )
#'
#' # Standard pie chart with default colours
#' plot_hc_pie(df, category = "category", value = "value", title = "Example")
#'
#' # Custom colours
#' plot_hc_pie(df, "category", "value",
#'             colors = c("#C40000", "#FF7D29", "#1482FA"))
#'
#' # Donut chart
#' plot_hc_pie(df, "category", "value", donut = TRUE, title = "Donut Example")
#' }
plot_hc_pie <- function(data,
                        category,
                        value,
                        title = NULL,
                        subtitle = NULL,
                        colors = NULL,
                        donut = FALSE,
                        show_labels = TRUE) {

  checkmate::assert_data_frame(data, min.cols = 2)
  checkmate::assert_string(category)
  checkmate::assert_string(value)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(subtitle, null.ok = TRUE)
  checkmate::assert_names(colnames(data), must.include = c(category, value))
  checkmate::assert_logical(donut, len = 1)
  checkmate::assert_logical(show_labels, len = 1)

  if (!is.null(colors)) {
    checkmate::assert_character(colors, min.len = length(unique(data[[category]])))
  }

  cat_sym <- rlang::sym(category)
  val_sym <- rlang::sym(value)

  pie_data <- data %>%
    dplyr::select(!!cat_sym, !!val_sym) %>%
    dplyr::rename(name = !!cat_sym, y = !!val_sym) %>%
    dplyr::mutate(y = as.numeric(y)) %>%
    dplyr::filter(!is.na(.data$y))

  if (!is.null(colors)) {
    pie_data <- pie_data %>%
      dplyr::mutate(color = rep(colors, length.out = dplyr::n()))
  }

  inner_size <- if (donut) "50%" else "0%"

  highcharter::highchart() %>%
    highcharter::hc_chart(type = "pie") %>%
    highcharter::hc_title(text = title, align = "center") %>%
    highcharter::hc_subtitle(text = subtitle, align = "center") %>%
    highcharter::hc_series(list(
      colorByPoint = is.null(colors),
      innerSize    = inner_size,
      data         = highcharter::list_parse(pie_data)
    )) %>%
    highcharter::hc_tooltip(
      pointFormat = "<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)"
    ) %>%
    highcharter::hc_plotOptions(
      pie = list(
        dataLabels = list(
          enabled = show_labels,
          format  = "{point.name} ({point.percentage:.1f}%)"
        )
      )
    )
}
