#' Histogram with Vertical Line Annotation
#'
#' Creates a histogram of a numeric variable and overlays a vertical reference
#' line (e.g., index date) with a text annotation.
#'
#' @param data A data frame containing the variable to plot.
#' @param numeric_variable A string specifying the column name of the numeric
#'   variable to plot.
#' @param binwidth Numeric width of histogram bins. Default `5`.
#' @param line_color Colour of the vertical reference line and label.
#'   Default `"#FF1F26"`.
#' @param line_text Text to annotate the vertical line. Default
#'   `"Index Date"`.
#' @param line_position Numeric x-intercept for the vertical line. Default `0`.
#' @param x_breaks_by Numeric spacing for x-axis breaks. Default `10`.
#' @param plot_title Optional string for the plot title. Defaults to
#'   `"Distribution of <numeric_variable>"`.
#' @param x_axis_label Optional string for the x-axis label. Defaults to the
#'   column name.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(days_since = rnorm(300, mean = 30, sd = 20))
#'
#' # Basic usage
#' plot_annotated_histogram(df, "days_since")
#'
#' # Custom labels and reference line
#' plot_annotated_histogram(
#'   data             = df,
#'   numeric_variable = "days_since",
#'   plot_title       = "Timeline Since Diagnosis",
#'   x_axis_label     = "Days Since Diagnosis",
#'   line_position    = 0,
#'   line_text        = "Index Date"
#' )
#' }
plot_annotated_histogram <- function(data,
                                     numeric_variable,
                                     binwidth      = 5,
                                     line_color    = "#FF1F26",
                                     line_text     = "Index Date",
                                     line_position = 0,
                                     x_breaks_by   = 10,
                                     plot_title    = NULL,
                                     x_axis_label  = NULL) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(numeric_variable)
  checkmate::assert_names(names(data), must.include = numeric_variable)
  checkmate::assert_numeric(data[[numeric_variable]])
  checkmate::assert_number(binwidth, lower = 0)
  checkmate::assert_string(line_color)
  checkmate::assert_string(line_text)
  checkmate::assert_number(line_position)
  checkmate::assert_number(x_breaks_by, lower = 1)
  checkmate::assert_string(plot_title, null.ok = TRUE)
  checkmate::assert_string(x_axis_label, null.ok = TRUE)

  #--- Set label defaults ---
  if (is.null(plot_title))  plot_title  <- paste("Distribution of", numeric_variable)
  if (is.null(x_axis_label)) x_axis_label <- numeric_variable

  #--- Compute axis range and histogram y-ceiling ---
  var_data <- data[[numeric_variable]]
  x_min    <- floor(min(var_data, na.rm = TRUE))
  x_max    <- ceiling(max(var_data, na.rm = TRUE))
  x_breaks <- seq(x_min, x_max, by = x_breaks_by)

  max_y <- max(
    ggplot2::ggplot_build(
      ggplot2::ggplot(data, ggplot2::aes(x = .data[[numeric_variable]])) +
        ggplot2::geom_histogram(binwidth = binwidth)
    )$data[[1]]$count,
    na.rm = TRUE
  )

  #--- Build plot ---
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[numeric_variable]])) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = "#1482FA", color = "black") +
    ggplot2::geom_vline(
      xintercept = line_position,
      linetype   = "dashed",
      color      = line_color,
      linewidth  = 1
    ) +
    ggplot2::annotate(
      "text",
      x     = line_position,
      y     = max_y,
      label = line_text,
      angle = 90,
      vjust = -0.5,
      hjust = 1,
      color = line_color,
      size  = 4
    ) +
    ggplot2::labs(title = plot_title, x = x_axis_label, y = "Frequency") +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
}
