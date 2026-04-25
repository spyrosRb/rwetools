#' Show Company Categorical Colour Palette
#'
#' Displays the company's 14-colour categorical palette as a labelled tile
#' plot, showing both the category name and hex code for each colour.
#'
#' @param title A string for the plot title. Default
#'   `"Company Categorical Palette"`.
#' @param ncol Number of columns to arrange swatches in. Default `7`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' \dontrun{
#' show_roche_palette()
#' show_roche_palette(ncol = 4)
#' }
show_roche_palette <- function(title = "Company Categorical Palette", ncol = 7) {

  #--- Argument checks ---
  checkmate::assert_string(title)
  checkmate::assert_integerish(ncol, lower = 1)

  #--- Define palette ---
  colors <- c(
    "Cat 01" = "#1482FA",
    "Cat 02" = "#F9AF90",
    "Cat 03" = "#983EBB",
    "Cat 04" = "#F47D34",
    "Cat 05" = "#0B24CB",
    "Cat 06" = "#87BDF7",
    "Cat 07" = "#D44040",
    "Cat 08" = "#AF9FE9",
    "Cat 09" = "#BB8377",
    "Cat 10" = "#F0B70D",
    "Cat 11" = "#32A6AE",
    "Cat 12" = "#F44390",
    "Cat 13" = "#C6BCB7",
    "Cat 14" = "#E2E1DE"
  )

  #--- Build tile data frame ---
  df <- tibble::tibble(
    label = names(colors),
    hex   = unname(colors),
    id    = seq_along(colors),
    col   = (id - 1) %% ncol + 1,
    row   = -((id - 1) %/% ncol + 1)
  )

  #--- Render palette plot ---
  ggplot2::ggplot(df, ggplot2::aes(x = col, y = row)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = hex),
      width = 0.95, height = 0.95, color = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(label, "\n", hex)),
      size = 3.2, color = "white", lineheight = 1.1
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
    )
}
