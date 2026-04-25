#' Calculate Change from Baseline
#'
#' Derives a baseline value and change from baseline for a numeric score column
#' in longitudinal data. Baseline is defined as the first non-missing
#' observation ordered by `visit`. All original rows and columns are preserved;
#' two new columns are appended.
#'
#' @param data A data frame containing longitudinal data.
#' @param patient_id A string specifying the column identifying each subject.
#' @param visit A string specifying the column identifying the visit or
#'   time point (used to determine ordering).
#' @param score A string specifying the numeric column for which baseline and
#'   change from baseline are calculated.
#' @param prefix An optional string prefix for the new column names. If
#'   supplied, columns are named `<prefix>_baseline` and `<prefix>_change`;
#'   otherwise `baseline` and `change`. Default `""`.
#'
#' @return The input data frame with two additional columns: baseline value
#'   and change from baseline. Rows where `visit` or `score` is `NA` receive
#'   `NA` for both new columns.
#' @export
#'
#' @examples
#' \dontrun{
#' weight_cfb <- weight_data %>%
#'   calc_change_from_baseline(
#'     patient_id = "subject_id",
#'     visit      = "visit_dt",
#'     score      = "weight_kg",
#'     prefix     = "weight"
#'   )%>%
  #' dplyr::filter(!is.na(change))
#' }
calc_change_from_baseline <- function(data, patient_id, visit, score, prefix = "") {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(patient_id)
  checkmate::assert_string(visit)
  checkmate::assert_string(score)
  checkmate::assert_string(prefix)
  checkmate::assert_names(names(data), must.include = c(patient_id, visit, score))

  #--- Determine output column names based on prefix ---
  baseline_var <- if (nzchar(prefix)) paste0(prefix, "_baseline") else "baseline"
  change_var   <- if (nzchar(prefix)) paste0(prefix, "_change")   else "change"

  #--- Derive baseline and change from baseline ---
  data |>
    dplyr::filter(!is.na(.data[[visit]]) & !is.na(.data[[score]])) |>
    dplyr::group_by(.data[[patient_id]]) |>
    dplyr::arrange(.data[[visit]], .by_group = TRUE) |>
    dplyr::mutate(
      "{baseline_var}" := dplyr::first(.data[[score]]),
      "{change_var}"   := .data[[score]] - .data[[baseline_var]]
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      .data[[patient_id]],
      .data[[visit]],
      .data[[score]],
      .data[[baseline_var]],
      .data[[change_var]]
    )
}
