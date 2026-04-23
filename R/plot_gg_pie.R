#' Static pie chart (ggplot2)
#'
#' Creates a pie or donut chart using ggplot2. Slice positions and label
#' coordinates are computed internally from `value`, so no pre-processing of
#' the data frame is required.
#'
#' @param data A data frame containing the data to be plotted.
#' @param category A string specifying the column name for slice categories.
#' @param value A string specifying the column name for numeric slice values.
#' @param title A string for the plot title. Default NULL.
#' @param palette_type Palette type passed to `assign_palette()`: `"diverging"`
#'   (default) or `"sequential"`. Ignored when `palette` is supplied.
#' @param palette A named character vector to override the auto palette
#'   (e.g. `c("A" = "#1482FA", "B" = "#FF1F26")`). If NULL, colours are
#'   assigned automatically via `assign_palette()`.
#' @param show_pct Logical. If TRUE (default), labels show category name and
#'   percentage. If FALSE, labels show category name only.
#' @param label_size Numeric size of slice labels. Default `4`.
#' @param label_threshold Minimum slice fraction (0–1) required to display a
#'   label. Suppresses labels on very small slices. Default `0.03`.
#' @param donut Logical. If TRUE, renders a donut chart. Default FALSE.
#' @param legend_position Legend position: `"none"` (default), `"right"`,
#'   `"bottom"`, etc. Useful when there are many categories.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   fruit = c("Apple", "Banana", "Cherry", "Date"),
#'   count = c(10, 20, 30, 15)
#' )
#'
#' # Standard pie chart
#' plot_gg_pie(df, category = "fruit", value = "count", title = "Fruit Counts")
#'
#' # Sequential auto palette
#' plot_gg_pie(df, category = "fruit", value = "count", palette_type = "sequential")
#'
#' # Manual palette override
#' plot_gg_pie(
#'   df,
#'   category = "fruit",
#'   value    = "count",
#'   palette  = c("Apple" = "#1482FA", "Banana" = "#FF1F26",
#'                "Cherry" = "#2ECC71", "Date" = "#F39C12")
#' )
#'
#' # Donut chart with legend instead of labels
#' plot_gg_pie(
#'   df,
#'   category        = "fruit",
#'   value           = "count",
#'   donut           = TRUE,
#'   show_pct        = FALSE,
#'   legend_position = "right"
#' )
plot_gg_pie <- function(data,
                        category,
                        value,
                        title = NULL,
                        palette_type = c("diverging", "sequential"),
                        palette = NULL,
                        show_pct = TRUE,
                        label_size = 4,
                        label_threshold = 0.03,
                        donut = FALSE,
                        legend_position = "none") {

  palette_type <- match.arg(palette_type)

  checkmate::assert_data_frame(data)
  checkmate::assert_string(category)
  checkmate::assert_string(value)
  checkmate::assert_logical(show_pct, len = 1)
  checkmate::assert_number(label_size, lower = 0)
  checkmate::assert_number(label_threshold, lower = 0, upper = 1)
  checkmate::assert_logical(donut, len = 1)
  checkmate::assert_string(legend_position)

  if (!category %in% names(data)) cli::cli_abort("Column {.val {category}} not found in data.")
  if (!value %in% names(data)) cli::cli_abort("Column {.val {value}} not found in data.")

  cat_sym <- ggplot2::sym(category)
  val_sym <- ggplot2::sym(value)

  x_inner <- if (donut) 0.5 else 0
  x_label <- (x_inner + 1) / 2

  data <- data %>%
    dplyr::mutate(
      fraction  = !!val_sym / sum(!!val_sym),
      ymax      = cumsum(fraction),
      ymin      = dplyr::lag(ymax, default = 0),
      label_pos = (ymax + ymin) / 2,
      label     = dplyr::if_else(
        fraction >= label_threshold,
        if (show_pct) {
          paste0(!!cat_sym, "\n", scales::percent(fraction, accuracy = 0.1))
        } else {
          as.character(!!cat_sym)
        },
        ""
      )
    )

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_rect(
      ggplot2::aes(
        ymin = ymin, ymax = ymax,
        xmin = x_inner, xmax = 1,
        fill = !!cat_sym
      ),
      colour = "white",
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x_label, y = label_pos, label = label),
      size = label_size
    ) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(0, 1)) +
    ggplot2::labs(title = title, fill = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = legend_position,
      plot.title      = ggplot2::element_text(hjust = 0.5, size = 13)
    )

  colours <- if (!is.null(palette)) palette else assign_palette(data, category, palette_type = palette_type)
  p <- p + ggplot2::scale_fill_manual(values = colours)

  p
}
