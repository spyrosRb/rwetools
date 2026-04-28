#' Static bar plot (ggplot2)
#'
#' Creates a bar plot using ggplot2.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x A string specifying the column name for the x-axis.
#' @param y A string specifying the column name for the y-axis.
#' @param color Fill colour for all bars. Default `"#1482FA"`.
#' @param show_legend Logical. If FALSE (default), hides the fill legend.
#' @param label_text_size Size of the bar label text. Default `3`.
#' @param title A string for the plot title. Default NULL.
#' @param subtitle A string for the plot subtitle. Default NULL.
#' @param x_label A string for the x-axis label. Default NULL (axis title hidden).
#' @param y_label A string for the y-axis label. Default NULL (axis title hidden).
#' @param x_text_angle Angle for x-axis text labels. Default `0`.
#' @param y_axis_format Format for y-axis labels: `"comma"`, `"percent"`, or
#'   `"abbreviated"`. Default `"comma"`.
#' @param y_limits Optional numeric vector of length 2 for y-axis limits.
#' @param y_breaks Optional numeric vector for y-axis breaks.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' # Abbreviated y-axis (10K, 25K, 50K)
#' df <- data.frame(category = c("A", "B", "C"), count = c(10000, 25000, 50000))
#' plot_gg_bar(df, x = "category", y = "count", y_axis_format = "abbreviated")
#'
#' # Thousands with comma separator (10,000 / 25,000 / 50,000)
#' plot_gg_bar(
#'   df,
#'   x            = "category",
#'   y            = "count",
#'   y_axis_format = "comma",
#'   y_limits     = c(0, 60000),
#'   y_breaks     = seq(0, 60000, by = 10000),
#'   title        = "Counts by Category",
#'   y_label      = "Total Count"
#' )
#'
#' # Percentage y-axis (0% to 100%)
#' df_pct <- data.frame(category = c("A", "B", "C"), pct = c(0.20, 0.45, 0.35))
#' plot_gg_bar(
#'   df_pct,
#'   x             = "category",
#'   y             = "pct",
#'   y_axis_format = "percent",
#'   y_limits      = c(0, 0.6),
#'   title         = "Share by Category",
#'   y_label       = "Percentage of Patients"
#' )
plot_gg_bar <- function(data,
                        x,
                        y,
                        color = "#1482FA",
                        show_legend = FALSE,
                        label_text_size = 3,
                        title = NULL,
                        subtitle = NULL,
                        x_label = NULL,
                        y_label = NULL,
                        x_text_angle = 0,
                        y_axis_format = c("comma", "percent", "abbreviated"),
                        y_limits = NULL,
                        y_breaks = NULL) {

  y_axis_format <- match.arg(y_axis_format)

  checkmate::assert_data_frame(data)
  checkmate::assert_string(x)
  checkmate::assert_string(y)
  checkmate::assert_string(color)
  checkmate::assert_number(label_text_size, lower = 1)
  checkmate::assert_logical(show_legend, len = 1)
  checkmate::assert_numeric(y_limits, len = 2, null.ok = TRUE)
  checkmate::assert_numeric(y_breaks, null.ok = TRUE)

  x_sym <- ggplot2::sym(x)
  y_sym <- ggplot2::sym(y)

  label_format <- switch(
    y_axis_format,
    "percent"      = scales::label_percent(),
    "comma"        = scales::label_comma(),
    "abbreviated"  = scales::label_number(scale_cut = scales::cut_short_scale())
  )

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
    ggplot2::geom_col(fill = color) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_format(!!y_sym)),
      vjust = -0.3,
      size = label_text_size,
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(
      labels = label_format,
      limits = y_limits,
      breaks = y_breaks,
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      x        = x_label,
      y        = y_label,
      fill     = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(
        angle = x_text_angle,
        hjust = if (x_text_angle != 0) 1 else 0.5
      )
    )

  p
}
