#' Plot Survival Curves
#'
#' Fits and plots Kaplan-Meier survival curves using `ggsurvfit`. Supports
#' confidence intervals, censor marks, a quantile reference line, and a risk
#' table.
#'
#' @param data A data frame containing time, status, and optional grouping
#'   columns.
#' @param time A string specifying the time-to-event column. Default `"time"`.
#' @param status A string specifying the event indicator column (0/1). Default
#'   `"status"`.
#' @param covariates An optional character vector of grouping/covariate column
#'   names. Default `NULL` (single overall curve).
#' @param title An optional string for the plot title. Default `NULL`.
#' @param xlab A string for the x-axis label. Default `"Time"`.
#' @param ylab A string for the y-axis label. Default
#'   `"Survival Probability"`.
#' @param xlim An optional numeric vector of length 2 for x-axis limits (e.g.
#'   `c(0, 8)`). Default `NULL`.
#' @param palette A character vector of colours for each stratum. Default
#'   `c("#1482FA", "#FF8782")`.
#' @param linewidth Line width of survival curves. Default `1`.
#' @param show_confint Logical. If TRUE, adds confidence interval ribbons.
#'   Default `TRUE`.
#' @param show_censor Logical. If TRUE, adds censor tick marks. Default
#'   `FALSE`.
#' @param show_quantile Logical. If TRUE, adds a horizontal reference line at
#'   `quantile_value`. Default `FALSE`.
#' @param quantile_value Numeric in (0, 1). Survival probability at which to
#'   draw the quantile reference line. Default `0.5` (median survival).
#' @param show_risktable Logical. If TRUE, adds a risk table below the plot.
#'   Default `FALSE`.
#' @param risktable_height Relative height of the risk table panel. Default
#'   `0.33`.
#' @param risktable_text_size Base text size for risk table statistics. Default
#'   `4`.
#' @param risktable_axis_size Axis text size for risk table strata labels.
#'   Default `11`.
#' @param risktable_title_size Title size within the risk table panel. Default
#'   `11`.
#' @param legend_position Legend position. Default `"bottom"`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(survival)
#' plot_survival(lung, time = "time", status = "status")
#'
#' plot_survival(lung, time = "time", status = "status",
#'               covariates = "sex", show_risktable = TRUE)
#' }
plot_survival <- function(data,
                          time                = "time",
                          status              = "status",
                          covariates          = NULL,
                          title               = NULL,
                          xlab                = "Time",
                          ylab                = "Survival Probability",
                          xlim                = NULL,
                          palette             = c("#1482FA", "#FF8782"),
                          linewidth           = 1,
                          show_confint        = TRUE,
                          show_censor         = FALSE,
                          show_quantile       = FALSE,
                          quantile_value      = 0.5,
                          show_risktable      = FALSE,
                          risktable_height    = 0.33,
                          risktable_text_size = 4,
                          risktable_axis_size = 11,
                          risktable_title_size = 11,
                          legend_position     = "bottom") {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(time, min.chars = 1)
  checkmate::assert_string(status, min.chars = 1)
  checkmate::assert_character(covariates, null.ok = TRUE)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(xlab)
  checkmate::assert_string(ylab)
  checkmate::assert_numeric(xlim, len = 2, null.ok = TRUE)
  checkmate::assert_character(palette, min.len = 1)
  checkmate::assert_number(linewidth, lower = 0)
  checkmate::assert_logical(show_confint, len = 1)
  checkmate::assert_logical(show_censor, len = 1)
  checkmate::assert_logical(show_quantile, len = 1)
  checkmate::assert_number(quantile_value, lower = 0, upper = 1)
  checkmate::assert_logical(show_risktable, len = 1)
  checkmate::assert_names(names(data), must.include = c(time, status))

  #--- Build survival formula and fit ---
  formula     <- build_surv_formula(time = time, status = status, covariates = covariates)
  surv_fit    <- ggsurvfit::survfit2(formula, data = data)

  #--- Base plot ---
  p <- surv_fit |>
    ggsurvfit::ggsurvfit(linewidth = linewidth) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = legend_position)

  #--- Optional layers ---
  if (show_risktable) {
    p <- p + ggsurvfit::add_risktable(
      risktable_height = risktable_height,
      size             = risktable_text_size,
      theme            = list(
        ggsurvfit::theme_risktable_default(
          axis.text.y.size = risktable_axis_size,
          plot.title.size  = risktable_title_size
        ),
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
      )
    )
  }

  if (show_confint)  p <- p + ggsurvfit::add_confidence_interval()
  if (show_censor)   p <- p + ggsurvfit::add_censor_mark()
  if (show_quantile) p <- p + ggsurvfit::add_quantile(y_value = quantile_value,
                                                       color = "grey30",
                                                       linewidth = 0.8)
  if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim)

  #--- Apply colour palette ---
  p +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    ggsurvfit::scale_ggsurvfit()
}
