#' UpSet plot (ggplot2 / ComplexUpset)
#'
#' Creates an UpSet plot using the ComplexUpset package. The first argument
#' must be a data frame whose set-membership columns contain binary values
#' (0/1 or TRUE/FALSE); it may also contain covariate columns.
#'
#' @param data A data frame with binary (0/1 or TRUE/FALSE) group-membership
#'   columns and optionally additional covariates.
#' @param intersect A character vector of column names that define set
#'   membership (e.g. `c("weight_baseline", "weight_wk26", "weight_wk52")`).
#' @param min_size Minimum intersection size to display. Default `0`.
#' @param n_intersections Maximum number of intersections to show, ordered by
#'   `sort_by`. Default `NULL` (all intersections).
#' @param color Fill colour for intersection bars. Default `"#1482FA"`.
#' @param set_sizes Logical. If TRUE, displays overall set-size bars on the
#'   left. Default `FALSE`.
#' @param show_total Logical. If TRUE, annotates the intersection panel with
#'   the total number of rows in `data`. Default `TRUE`.
#' @param label_size Size of the bar text labels (percentage + count). Default `3`.
#' @param width_ratio Ratio of set-matrix width to intersection bar width.
#'   Default `0.1`.
#' @param title A string for the plot title. Default `NULL`.
#' @param subtitle A string for the plot subtitle. Default `NULL`.
#'
#' @return A `patchwork` object produced by `ComplexUpset::upset()`.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 200
#' df <- data.frame(
#'   weight_baseline = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.9, 0.1)),
#'   weight_wk26     = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.7, 0.3)),
#'   weight_wk52     = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.5, 0.5))
#' )
#'
#' plot_gg_upset(
#'   df,
#'   intersect = c("weight_baseline", "weight_wk26", "weight_wk52"),
#'   title     = "Weight Measurement Availability"
#' )
#' }
plot_gg_upset <- function(data,
                          intersect,
                          min_size = 0,
                          n_intersections = NULL,
                          color = "#1482FA",
                          set_sizes = FALSE,
                          show_total = TRUE,
                          label_size = 3,
                          width_ratio = 0.1,
                          title = NULL,
                          subtitle = NULL) {

  checkmate::assert_data_frame(data)
  checkmate::assert_character(intersect, min.len = 1)
  checkmate::assert_number(min_size, lower = 0)
  checkmate::assert_count(n_intersections, null.ok = TRUE)
  checkmate::assert_string(color)
  checkmate::assert_logical(set_sizes, len = 1)
  checkmate::assert_logical(show_total, len = 1)
  checkmate::assert_number(label_size, lower = 0)
  checkmate::assert_number(width_ratio, lower = 0, upper = 1)

  missing_cols <- setdiff(intersect, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Column{?s} not found in data: {.val {missing_cols}}")
  }

  bar_annotation <- ComplexUpset::intersection_size(
    counts       = TRUE,
    mapping      = ggplot2::aes(fill = "bars_color"),
    text_mapping = ggplot2::aes(
      label = paste0(
        !!ComplexUpset::upset_text_percentage(),
        "\n(",
        format(!!ComplexUpset::get_size_mode("exclusive_intersection"), big.mark = ","),
        ")"
      )
    ),
    text = list(
      size     = label_size,
      fontface = "bold",
      vjust    = -0.2
    )
  ) +
    ggplot2::scale_fill_manual(
      values = c("bars_color" = color),
      guide  = "none"
    )

  if (show_total) {
    bar_annotation <- bar_annotation +
      ggplot2::annotate(
        geom     = "text",
        x        = Inf,
        y        = Inf,
        label    = paste("N obs:", format(nrow(data), big.mark = ",")),
        vjust    = 1,
        hjust    = 1.5,
        size     = 4,
        fontface = "bold"
      )
  }

  p <- ComplexUpset::upset(
    data             = data,
    intersect        = intersect,
    min_size         = min_size,
    n_intersections  = n_intersections,
    set_sizes        = set_sizes,
    base_annotations = list(
      "Intersection size" = bar_annotation
    ),
    themes = ComplexUpset::upset_modify_themes(list(
      "intersections_matrix" = ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ),
      "overall_sizes"     = ggplot2::theme(axis.title.y = ggplot2::element_blank()),
      "Intersection size" = ggplot2::theme()
    )),
    width_ratio = width_ratio
  )

  if (!is.null(title) || !is.null(subtitle)) {
    p <- p + patchwork::plot_annotation(title = title, subtitle = subtitle)
  }

  p
}
