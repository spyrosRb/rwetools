#' Plot Distribution of Laboratory Values
#'
#' Creates a combined histogram and horizontal boxplot to visualise the
#' distribution of a numeric laboratory value. Useful for identifying shape,
#' spread, outliers, and central tendency.
#'
#' @param data A data frame containing the lab value column.
#' @param lab_var A string specifying the column name of the lab value.
#' @param binwidth Width of histogram bins. Default `0.5`.
#' @param title Optional plot title. Defaults to
#'   `"Distribution of <lab_var>"`.
#' @param fill_color Colour for histogram bars and boxplot fill.
#'   Default `"#1482FA"`.
#' @param x_breaks_by Interval between x-axis ticks. Default `2`.
#'
#' @return A `cowplot` object combining histogram and boxplot.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(HbA1c = rnorm(200, mean = 7, sd = 1.2))
#' plot_lab_distribution(df, "HbA1c")
#'
#' plot_lab_distribution(df, "HbA1c", binwidth = 0.2,
#'                       title = "HbA1c Distribution", x_breaks_by = 1)
#' }
plot_lab_distribution <- function(data,
                                   lab_var,
                                   binwidth    = 0.5,
                                   title       = NULL,
                                   fill_color  = "#1482FA",
                                   x_breaks_by = 2) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(lab_var)
  checkmate::assert_names(names(data), must.include = lab_var)
  checkmate::assert_numeric(data[[lab_var]], any.missing = FALSE)
  checkmate::assert_number(binwidth, lower = 0)
  checkmate::assert_string(fill_color)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_number(x_breaks_by, lower = 0.1)

  #--- Defaults and axis range ---
  if (is.null(title)) title <- paste("Distribution of", lab_var)

  lab_vals <- data[[lab_var]]
  x_min    <- floor(min(lab_vals, na.rm = TRUE)) - 1
  x_max    <- ceiling(max(lab_vals, na.rm = TRUE)) + 1
  x_limits <- c(x_min, x_max)
  x_breaks <- seq(x_min, x_max, by = x_breaks_by)

  #--- Histogram ---
  hist_plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[lab_var]])) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = fill_color, color = "black") +
    ggplot2::labs(title = title, x = NULL, y = "Frequency") +
    ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title.x     = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(5, 5, 2, 5)
    )

  #--- Boxplot ---
  box_plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[lab_var]], y = factor(1))) +
    ggplot2::geom_boxplot(
      fill           = fill_color,
      color          = "black",
      width          = 0.8,
      outlier.shape  = 21,
      outlier.fill   = "#FF1F26",
      outlier.color  = "#FF1F26",
      outlier.size   = 2.5
    ) +
    ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    ggplot2::labs(x = lab_var, y = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid   = ggplot2::element_blank(),
      plot.margin  = ggplot2::margin(2, 5, 5, 5)
    )

  #--- Combine panels ---
  cowplot::plot_grid(
    hist_plot, box_plot,
    ncol        = 1,
    align       = "v",
    rel_heights = c(3, 0.6),
    axis        = "lr",
    greedy      = TRUE
  )
}
