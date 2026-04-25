#' Correlation Heatmap (Static or Interactive)
#'
#' Computes a correlation matrix and plots a heatmap. Supports both a static
#' `ggcorrplot` visualization and an interactive `highcharter` visualization.
#'
#' @param data A data frame containing numeric variables.
#' @param method Correlation method: `"pearson"` (default), `"kendall"`, or
#'   `"spearman"`.
#' @param focus_vars Optional character vector of column names to subset before
#'   computing correlation. Default `NULL`.
#' @param title A string for the plot title. Default `"Correlation Heatmap"`.
#' @param plot_method Shape for static plot cells: `"circle"` or `"square"`
#'   (default). Static only.
#' @param show_legend Logical. Whether to show the legend. Default `TRUE`.
#'   Static only.
#' @param colors A character vector of three colours for low, mid, and high
#'   correlation. Default `c("#0b41cd", "#ffffff", "#FF1F26")`. Static only.
#' @param lab Logical. If TRUE, shows correlation values on cells. Default
#'   `TRUE`. Static only.
#' @param lab_col Label colour. Default `"black"`. Static only.
#' @param lab_size Label size. Default `3`. Static only.
#' @param digits Number of decimal places to round correlation values. Default
#'   `2`.
#' @param interactive Logical. If TRUE, returns an interactive `highcharter`
#'   heatmap. Default `FALSE`.
#'
#' @return A `ggplot` or `highchart` object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Static heatmap
#' plot_correlation_heatmap(mtcars, method = "pearson")
#'
#' # Interactive heatmap
#' plot_correlation_heatmap(mtcars, method = "spearman", interactive = TRUE)
#'
#' # Focused variables
#' plot_correlation_heatmap(mtcars, focus_vars = c("mpg", "wt", "hp"))
#' }
plot_correlation_heatmap <- function(data,
                                     method      = "pearson",
                                     focus_vars  = NULL,
                                     title       = "Correlation Heatmap",
                                     plot_method = "square",
                                     show_legend = TRUE,
                                     colors      = c("#0b41cd", "#ffffff", "#FF1F26"),
                                     lab         = TRUE,
                                     lab_col     = "black",
                                     lab_size    = 3,
                                     digits      = 2,
                                     interactive = FALSE) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_choice(method, c("pearson", "kendall", "spearman"))
  checkmate::assert_character(focus_vars, null.ok = TRUE)
  checkmate::assert_string(title)
  checkmate::assert_choice(plot_method, c("circle", "square"))
  checkmate::assert_logical(show_legend, len = 1)
  checkmate::assert_character(colors, len = 3)
  checkmate::assert_logical(lab, len = 1)
  checkmate::assert_string(lab_col)
  checkmate::assert_number(lab_size, lower = 0)
  checkmate::assert_integerish(digits, lower = 0)
  checkmate::assert_logical(interactive, len = 1)

  if (!is.null(focus_vars)) {
    checkmate::assert_names(names(data), must.include = focus_vars)
  }

  #--- Subset and compute correlation ---
  if (!is.null(focus_vars)) {
    data <- dplyr::select(data, dplyr::all_of(focus_vars))
  }

  corr_matrix <- stats::cor(data, method = method, use = "pairwise.complete.obs")

  #--- Render plot ---
  if (interactive) {
    highcharter::hchart(corr_matrix) |>
      highcharter::hc_title(text = title) |>
      highcharter::hc_tooltip(valueDecimals = digits, shared = TRUE) |>
      highcharter::hc_exporting(enabled = TRUE) |>
      highcharter::hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled   = TRUE,
            formatter = highcharter::JS(
              paste0("function(){ return Highcharts.numberFormat(this.point.value, ", digits, "); }")
            )
          )
        )
      )
  } else {
    ggcorrplot::ggcorrplot(
      corr          = corr_matrix,
      method        = plot_method,
      outline.color = "white",
      ggtheme       = ggplot2::theme_minimal,
      title         = title,
      show.legend   = show_legend,
      legend.title  = "Correlation",
      colors        = colors,
      lab           = lab,
      lab_col       = lab_col,
      lab_size      = lab_size,
      digits        = digits
    ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )
  }
}
