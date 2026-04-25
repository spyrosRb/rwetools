#' Calculate Correlation Matrix
#'
#' Computes a correlation matrix from a data frame using the specified method
#' (Pearson, Spearman, or Kendall) via `corrr::correlate()`. Optionally
#' reduces to a subset of variables using `corrr::focus()`.
#'
#' @param data A data frame containing numeric variables to correlate.
#' @param method Correlation method: `"pearson"` (default), `"spearman"`, or
#'   `"kendall"`.
#' @param focus_vars Optional character vector of variable names to focus on
#'   via `corrr::focus()`. Default `NULL`.
#' @param quiet Logical. If TRUE, suppresses messages during computation.
#'   Default `TRUE`.
#'
#' @return A correlation tibble (`cor_df`) from `corrr::correlate()`,
#'   optionally filtered by `corrr::focus()`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Full correlation matrix
#' calc_correlation_matrix(mtcars, method = "pearson")
#'
#' # Focused on specific variables
#' calc_correlation_matrix(mtcars, method = "spearman",
#'                         focus_vars = c("mpg", "hp"))
#' }
calc_correlation_matrix <- function(data,
                                    method     = "pearson",
                                    focus_vars = NULL,
                                    quiet      = TRUE) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data, all.missing = FALSE)
  checkmate::assert_choice(method, choices = c("pearson", "kendall", "spearman"))
  checkmate::assert_flag(quiet)
  checkmate::assert_character(focus_vars, null.ok = TRUE)

  if (!is.null(focus_vars)) {
    checkmate::assert_names(names(data), must.include = focus_vars)
  }

  #--- Compute correlation matrix ---
  corr_matrix <- corrr::correlate(data, method = method, quiet = quiet)

  #--- Optionally reduce to focus variables ---
  if (!is.null(focus_vars)) {
    corr_matrix <- corrr::focus(corr_matrix, focus_vars)
  }

  corr_matrix
}
