#' Assign a colour palette based on number of categories
#'
#' Assigns a colour palette to a categorical variable based on the number of
#' distinct categories. Supports two palette types: `"diverging"` (blue-orange)
#' and `"sequential"` (blue shades). For 1 or 2 categories specific palettes
#' are returned; for larger counts the closest available palette size is used.
#'
#' @param data A data frame containing the categorical data.
#' @param column A string specifying the column name with categorical values.
#' @param palette_type Palette type: `"diverging"` (default) or `"sequential"`.
#'
#' @return A character vector of hex colour codes.
#' @export
#'
#' @examples
#' df <- data.frame(country = c("USA", "Canada", "Mexico", "Greece"))
#' assign_palette(df, "country", palette_type = "diverging")
#' assign_palette(df, "country", palette_type = "sequential")
assign_palette <- function(data,
                           column,
                           palette_type = c("diverging", "sequential")) {

  palette_type <- match.arg(palette_type)

  checkmate::assert_data_frame(data)
  checkmate::assert_string(column)
  checkmate::assert_choice(palette_type, choices = c("diverging", "sequential"))

  if (!column %in% names(data)) cli::cli_abort("Column {.val {column}} not found in data.")

  unique_n <- length(unique(data[[column]]))

  if (unique_n == 1) return("#1482FA")
  if (unique_n == 2) return(c("#3685F1", "#F44542"))

  diverging_palettes <- list(
    `3`  = c("#247DE2", "#EAE8E4", "#E74106"),
    `5`  = c("#072163", "#247DE2", "#EAE8E4", "#E74106", "#530B00"),
    `7`  = c("#072163", "#247DE2", "#B7E8FF", "#EAE8E4", "#FFDBAB", "#E74106", "#530B00"),
    `9`  = c("#072163", "#215BBC", "#4BA3EB", "#B7E8FF", "#EAE8E4", "#FFDBAB", "#FC762F", "#B62801", "#530B00"),
    `11` = c("#072163", "#1D4BA6", "#247DE2", "#67B5EC", "#B7E8FF", "#EAE8E4", "#FFDBAB", "#FE924A", "#E74106", "#9C2000", "#530B00"),
    `13` = c("#072163", "#1A4299", "#2368CD", "#3594EB", "#77C0EF", "#B7E8FF", "#EAE8E4", "#FFDBAB", "#FFA25A", "#F85E1A", "#CA3002", "#8D1B00", "#530B00"),
    `15` = c("#072163", "#173C90", "#215BBC", "#247DE2", "#4BA3EB", "#81C7F1", "#B7E8FF", "#EAE8E4", "#FFDBAB", "#FFAC66", "#FC762F", "#E74106", "#B62801", "#831800", "#530B00"),
    `17` = c("#072163", "#153889", "#1F52AF", "#236ED4", "#2E8EEA", "#5CAEEB", "#89CBF3", "#B7E8FF", "#EAE8E4", "#FFDBAB", "#FFB36F", "#FD873F", "#F45512", "#D33403", "#A72300", "#7C1600", "#530B00"),
    `19` = c("#072163", "#133584", "#1D4BA6", "#2263C7", "#247DE2", "#3D9AEB", "#67B5EC", "#8FCFF4", "#B7E8FF", "#EAE8E4", "#FFDBAB", "#FFB876", "#FE924A", "#FA6721", "#E74106", "#C32D01", "#9C2000", "#771500", "#530B00"),
    `21` = c("#072163", "#123380", "#1B469F", "#215BBC", "#2371D7", "#2A8AE9", "#4BA3EB", "#70BBEE", "#93D2F5", "#B7E8FF", "#EAE8E4", "#FFDBAB", "#FFBC7B", "#FF9B52", "#FC762F", "#F2500E", "#D73703", "#B62801", "#941D00", "#731400", "#530B00")
  )

  sequential_palettes <- list(
    `3`  = c("#B6D9F2", "#3685F1", "#0936AB"),
    `4`  = c("#B6D9F2", "#56A4F0", "#0A68E1", "#0936AB"),
    `5`  = c("#B6D9F2", "#6EB2F0", "#3685F1", "#0359D9", "#0936AB"),
    `6`  = c("#B6D9F2", "#7ABAF0", "#4498F0", "#4498F0", "#0150D2", "#0936AB"),
    `7`  = c("#B6D9F2", "#83BFF0", "#56A4F0", "#3685F1", "#0A68E1", "#004BCC", "#0936AB"),
    `8`  = c("#B6D9F2", "#89C2F0", "#64ACF0", "#3F93F1", "#2579EB", "#0460DD", "#0047C8", "#0936AB"),
    `9`  = c("#B6D9F2", "#8EC5F0", "#6EB2F0", "#4A9DF0", "#3685F1", "#1470E5", "#0359D9", "#0144C4", "#0936AB"),
    `10` = c("#B6D9F2", "#92C7F0", "#75B6F0", "#56A4F0", "#3D90F1", "#297BED", "#0A68E1", "#0254D5", "#0242C1", "#0936AB")
  )

  palette_set    <- switch(palette_type, diverging = diverging_palettes, sequential = sequential_palettes)
  available_sizes <- as.integer(names(palette_set))

  best_size <- if (palette_type == "diverging") {
    sizes_above <- available_sizes[available_sizes >= unique_n]
    if (length(sizes_above) == 0) available_sizes[length(available_sizes)] else sizes_above[[1]]
  } else {
    available_sizes[which.min(abs(available_sizes - unique_n))]
  }

  palette_set[[as.character(best_size)]]
}
