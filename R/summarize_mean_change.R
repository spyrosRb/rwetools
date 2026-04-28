#' Summarize Mean Change from Baseline by Visit
#'
#' Calculates the mean change from baseline for a score variable by visit,
#' optionally computing adjusted means using the `emmeans` package. Visits
#' with fewer than 4 subjects trigger a warning.
#'
#' @param data A data frame containing the data.
#' @param patient_id A string specifying the column identifying each subject.
#' @param visit A string specifying the visit or time point column.
#' @param score_change A string specifying the column representing change from
#'   baseline.
#' @param adjustment_vars A character vector of covariate column names to
#'   adjust for in the linear model. Required when `use_adjusted_means = TRUE`.
#'   Default `NULL`.
#' @param use_adjusted_means Logical. If TRUE, computes adjusted means via
#'   `emmeans`. Default `FALSE`.
#' @param conf_level Confidence level for confidence intervals. Default `0.95`.
#' @param na_rm Logical. If TRUE, removes NAs when calculating means.
#'   Default `TRUE`.
#'
#' @return A data frame with columns: visit, n, raw_mean, raw_ci_lower,
#'   raw_ci_upper, and (when `use_adjusted_means = TRUE`) adjusted_mean,
#'   adj_ci_lower, adj_ci_upper.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' df <- data.frame(
#'   patient_id    = rep(1:50, each = 3),
#'   visit         = rep(c("Baseline", "Week 26", "Week 52"), times = 50),
#'   weight_change = c(rep(0, 50), rnorm(50, mean = -2, sd = 1.5), rnorm(50, mean = -4, sd = 2)),
#'   age           = rep(sample(40:70, 50, replace = TRUE), each = 3),
#'   sex           = rep(sample(c("M", "F"), 50, replace = TRUE), each = 3)
#' )
#'
#' # Raw means only
#' summarize_mean_change(
#'   data         = df,
#'   patient_id   = "patient_id",
#'   visit        = "visit",
#'   score_change = "weight_change"
#' )
#'
#' # With adjusted means
#' summarize_mean_change(
#'   data               = df,
#'   patient_id         = "patient_id",
#'   visit              = "visit",
#'   score_change       = "weight_change",
#'   adjustment_vars    = c("age", "sex"),
#'   use_adjusted_means = TRUE
#' )
#' }
summarize_mean_change <- function(data,
                                  patient_id,
                                  visit,
                                  score_change,
                                  adjustment_vars    = NULL,
                                  use_adjusted_means = FALSE,
                                  conf_level         = 0.95,
                                  na_rm              = TRUE) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(patient_id)
  checkmate::assert_string(visit)
  checkmate::assert_string(score_change)
  checkmate::assert_character(adjustment_vars, null.ok = TRUE)
  checkmate::assert_logical(use_adjusted_means, len = 1)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_logical(na_rm, len = 1)
  checkmate::assert_names(names(data), must.include = c(patient_id, visit, score_change))

  if (use_adjusted_means && is.null(adjustment_vars)) {
    cli::cli_abort(
      "When {.arg use_adjusted_means} is TRUE, {.arg adjustment_vars} must be provided."
    )
  }

  #--- Calculate raw means by visit ---
  raw_means <- data |>
    dplyr::group_by(.data[[visit]]) |>
    dplyr::summarise(
      n        = dplyr::n_distinct(.data[[patient_id]]),
      raw_mean = round(mean(.data[[score_change]], na.rm = na_rm), 2),
      raw_sd   = round(stats::sd(.data[[score_change]], na.rm = na_rm), 2),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      se           = raw_sd / sqrt(n),
      t_value      = stats::qt((1 + conf_level) / 2, df = n - 1),
      raw_ci_lower = round(raw_mean - t_value * se, 2),
      raw_ci_upper = round(raw_mean + t_value * se, 2)
    ) |>
    dplyr::select(-raw_sd, -se, -t_value)

  small_n_visits <- raw_means |>
    dplyr::filter(n < 4) |>
    dplyr::pull(.data[[visit]])

  if (length(small_n_visits) > 0) {
    cli::cli_warn(
      "Small sample size (n < 4) at visit(s): {.val {small_n_visits}}. Interpret adjusted means with caution."
    )
  }

  if (!use_adjusted_means) {
    return(raw_means)
  }

  #--- Calculate adjusted means using emmeans ---
  formula <- stats::as.formula(
    paste(score_change, "~", visit, "+", paste(adjustment_vars, collapse = " + "))
  )

  adjusted_means <- stats::lm(formula, data = data) |>
    emmeans::emmeans(specs = visit, level = conf_level) |>
    summary() |>
    as.data.frame() |>
    dplyr::rename(
      adjusted_mean = emmean,
      adj_ci_lower  = lower.CL,
      adj_ci_upper  = upper.CL
    ) %>% 
    dplyr::mutate(
      adjusted_mean = round(adjusted_mean, 2),
      adj_ci_lower  = round(adj_ci_lower,2),
      adj_ci_upper  = round(adj_ci_upper,2)
    ) %>% 
    dplyr::select(
      dplyr::all_of(
        c(visit, 
          'adjusted_mean', 
          'adj_ci_lower', 
          'adj_ci_upper')
      )
    )
   
  #--- Merge raw and adjusted means ---
  raw_means |>
    dplyr::left_join(adjusted_means, by = visit) |>
    dplyr::select(
      dplyr::all_of(visit), n,
      raw_mean, raw_ci_lower, raw_ci_upper,
      adjusted_mean, adj_ci_lower, adj_ci_upper
    )
}
