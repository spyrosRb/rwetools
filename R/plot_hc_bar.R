#' Interactive bar plot (Highcharts)
#'
#' Creates an interactive bar plot using the highcharter package.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x A string specifying the column name for the x-axis.
#' @param y A string specifying the column name for the y-axis.
#' @param color A string specifying the bar fill colour. Default `"#1482FA"`.
#' @param show_labels Logical. If TRUE, displays formatted data labels on bars.
#'   Default FALSE.
#' @param label_text_size Size of data label text in pixels. Default `11`.
#' @param title A string for the plot title. Default NULL.
#' @param subtitle A string for the plot subtitle. Default NULL.
#' @param x_label A string for the x-axis label. Default NULL.
#' @param y_label A string for the y-axis label. Default NULL.
#' @param x_text_angle Rotation angle for x-axis labels in degrees. Default `0`.
#' @param y_axis_format Format for y-axis and data labels: `"comma"` (default),
#'   `"percent"`, or `"abbreviated"`.
#' @param flip Logical. If TRUE, renders a horizontal bar chart. Default FALSE.
#'
#' @return A `highchart` object.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   country = c("Germany", "Belgium", "Austria", "Sweden", "Spain", "Italy"),
#'   value   = c(500000, 450000, 150000, 200000, 350000, 250000)
#' )
#'
#' # Abbreviated y-axis
#' plot_hc_bar(df, x = "country", y = "value", y_axis_format = "abbreviated",
#'             title = "GDP by Country", x_text_angle = 45)
#'
#' # With data labels
#' plot_hc_bar(df, x = "country", y = "value", show_labels = TRUE,
#'             y_axis_format = "comma")
#'
#' # Horizontal bars
#' plot_hc_bar(df, x = "country", y = "value", flip = TRUE,
#'             y_axis_format = "abbreviated")
#' }
plot_hc_bar <- function(data,
                        x,
                        y,
                        color = "#1482FA",
                        show_labels = FALSE,
                        label_text_size = 11,
                        title = NULL,
                        subtitle = NULL,
                        x_label = NULL,
                        y_label = NULL,
                        x_text_angle = 0,
                        y_axis_format = c("comma", "percent", "abbreviated"),
                        flip = FALSE) {

  y_axis_format <- match.arg(y_axis_format)

  checkmate::assert_data_frame(data)
  checkmate::assert_string(x)
  checkmate::assert_string(y)
  checkmate::assert_string(color)
  checkmate::assert_logical(show_labels, len = 1)
  checkmate::assert_number(label_text_size, lower = 1)
  checkmate::assert_logical(flip, len = 1)

  if (!x %in% names(data)) cli::cli_abort("Column {.val {x}} not found in data.")
  if (!y %in% names(data)) cli::cli_abort("Column {.val {y}} not found in data.")

  x_sym <- rlang::sym(x)
  y_sym <- rlang::sym(y)

  chart_type <- if (flip) "bar" else "column"

  axis_formatter <- switch(
    y_axis_format,
    "percent" = highcharter::JS(
      "function() { return Highcharts.numberFormat(this.value * 100, 1) + '%'; }"
    ),
    "abbreviated" = highcharter::JS(
      "function() {
        var n = this.value;
        if (n >= 1e9) return Highcharts.numberFormat(n / 1e9, 1) + 'B';
        if (n >= 1e6) return Highcharts.numberFormat(n / 1e6, 1) + 'M';
        if (n >= 1e3) return Highcharts.numberFormat(n / 1e3, 1) + 'K';
        return Highcharts.numberFormat(n, 0);
      }"
    ),
    "comma" = highcharter::JS(
      "function() { return Highcharts.numberFormat(this.value, 0, '.', ','); }"
    )
  )

  tooltip_formatter <- switch(
    y_axis_format,
    "percent" = highcharter::JS(
      "function() {
        return '<b>' + this.point.name + '</b>: ' +
               Highcharts.numberFormat(this.y * 100, 1) + '%';
      }"
    ),
    "abbreviated" = highcharter::JS(
      "function() {
        var n = this.y, label;
        if (n >= 1e9)      label = Highcharts.numberFormat(n / 1e9, 1) + 'B';
        else if (n >= 1e6) label = Highcharts.numberFormat(n / 1e6, 1) + 'M';
        else if (n >= 1e3) label = Highcharts.numberFormat(n / 1e3, 1) + 'K';
        else               label = Highcharts.numberFormat(n, 0);
        return '<b>' + this.point.name + '</b>: ' + label;
      }"
    ),
    "comma" = highcharter::JS(
      "function() {
        return '<b>' + this.point.name + '</b>: ' +
               Highcharts.numberFormat(this.y, 0, '.', ',');
      }"
    )
  )

  label_formatter <- switch(
    y_axis_format,
    "percent" = highcharter::JS(
      "function() { return Highcharts.numberFormat(this.y * 100, 1) + '%'; }"
    ),
    "abbreviated" = highcharter::JS(
      "function() {
        var n = this.y;
        if (n >= 1e9) return Highcharts.numberFormat(n / 1e9, 1) + 'B';
        if (n >= 1e6) return Highcharts.numberFormat(n / 1e6, 1) + 'M';
        if (n >= 1e3) return Highcharts.numberFormat(n / 1e3, 1) + 'K';
        return Highcharts.numberFormat(n, 0);
      }"
    ),
    "comma" = highcharter::JS(
      "function() { return Highcharts.numberFormat(this.y, 0, '.', ','); }"
    )
  )

  hc <- highcharter::hchart(
    data,
    chart_type,
    highcharter::hcaes(x = !!x_sym, y = !!y_sym),
    color = color
  ) %>%
    highcharter::hc_title(text = title, align = "center") %>%
    highcharter::hc_subtitle(text = subtitle, align = "center") %>%
    highcharter::hc_xAxis(
      title  = list(text = x_label),
      labels = list(rotation = x_text_angle),
      lineColor = "#0000001A"
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = y_label),
      gridLineDashStyle = "Dash",
      labels = list(formatter = axis_formatter)
    ) %>%
    highcharter::hc_tooltip(formatter = tooltip_formatter) %>%
    highcharter::hc_plotOptions(
      series = list(
        dataLabels = list(
          enabled   = show_labels,
          formatter = label_formatter,
          style     = list(fontSize = paste0(label_text_size, "px"))
        )
      )
    )

  hc
}
