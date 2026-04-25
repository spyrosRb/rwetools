#' Compute Comorbidity Scores for a Patient Cohort
#'
#' Calculates Charlson or Elixhauser comorbidity scores from diagnosis data
#' using ICD-9 and/or ICD-10 codes via the \pkg{comorbidity} package.
#' When both ICD versions are supplied, per-patient flags are combined by
#' taking the maximum across versions before scoring.
#'
#' @param data A data frame containing diagnosis data.
#' @param patient_id A string specifying the patient ID column. Default
#'   `"patient_id"`.
#' @param diagnosis_code A string specifying the diagnosis code column. Default
#'   `"diagnosis_code"`.
#' @param diagnosis_date A string specifying the diagnosis date column. Default
#'   `"diagnosis_date"`.
#' @param index_date A string specifying the index date column. Default
#'   `"index_date"`.
#' @param lookback_days Number of days before the index date to include
#'   diagnoses. Default `365`.
#' @param followup_days Number of days after the index date to include
#'   diagnoses. Default `0`.
#' @param icd A character vector: one or both of `"icd9"`, `"icd10"`. Default
#'   `c("icd9", "icd10")`.
#' @param method Scoring method: `"charlson"` (default) or `"elixhauser"`.
#' @param tidy.codes Logical. If `TRUE` (default), ICD codes are uppercased and
#'   non-alphanumeric characters removed before matching.
#'
#' @return A tibble with one row per patient containing comorbidity flags, raw
#'   score, weighted score, and index category (Charlson only).
#' @export
#'
#' @examples
#' \dontrun{
#' compute_comorbidities(dx_data, method = "charlson")
#' compute_comorbidities(dx_data, icd = "icd10", method = "elixhauser")
#' }
compute_comorbidities <- function(data,
                                  patient_id     = "patient_id",
                                  diagnosis_code = "diagnosis_code",
                                  diagnosis_date = "diagnosis_date",
                                  index_date     = "index_date",
                                  lookback_days  = 365,
                                  followup_days  = 0,
                                  icd            = c("icd9", "icd10"),
                                  method         = c("charlson", "elixhauser"),
                                  tidy.codes     = TRUE) {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_string(patient_id, min.chars = 1)
  checkmate::assert_string(diagnosis_code, min.chars = 1)
  checkmate::assert_string(diagnosis_date, min.chars = 1)
  checkmate::assert_string(index_date, min.chars = 1)
  checkmate::assert_count(lookback_days, positive = TRUE)
  checkmate::assert_count(followup_days)
  checkmate::assert_logical(tidy.codes, len = 1)
  checkmate::assert_names(names(data),
                          must.include = c(patient_id, diagnosis_code,
                                           diagnosis_date, index_date))
  method <- match.arg(method)
  icd    <- match.arg(icd, several.ok = TRUE)

  #--- Coerce date columns in-place ---
  dx_data <- data |>
    dplyr::mutate(
      "{diagnosis_date}" := lubridate::as_date(.data[[diagnosis_date]]),
      "{index_date}"     := lubridate::as_date(.data[[index_date]])
    )

  #--- Optionally clean ICD codes ---
  if (tidy.codes) {
    dx_data <- dx_data |>
      dplyr::mutate(
        "{diagnosis_code}" := toupper(
          gsub("[^[:alnum:]]", "", .data[[diagnosis_code]])
        )
      )
  }

  #--- Filter by lookback/followup window and deduplicate ---
  dx_filtered <- dx_data |>
    dplyr::filter(
      .data[[diagnosis_date]] >= (.data[[index_date]] - lookback_days) &
        .data[[diagnosis_date]] <= (.data[[index_date]] + followup_days)
    ) |>
    dplyr::distinct(.data[[patient_id]], .data[[diagnosis_code]],
                    .data[[diagnosis_date]])

  #--- Compute comorbidity flags for each ICD version then combine ---
  comorb_list <- purrr::map(icd, function(icd_type) {
    comorbidity::comorbidity(
      x          = dx_filtered,
      id         = patient_id,
      code       = diagnosis_code,
      map        = paste0(method, "_", icd_type, "_quan"),
      assign0    = FALSE,
      tidy.codes = FALSE
    )
  })

  comorbid <- data.table::rbindlist(comorb_list)
  comorbid <- comorbid[, lapply(.SD, max, na.rm = TRUE), by = patient_id]

  #--- Compute score, weighted score, and index category (Charlson only) ---
  if (method == "charlson") {
    charlson_cols <- c("mi", "chf", "pvd", "cevd", "dementia", "cpd", "rheumd",
                       "pud", "mld", "diab", "diabwc", "hp", "rend", "canc",
                       "msld", "metacanc", "aids")
    weights       <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 6, 6)
    names(weights) <- charlson_cols

    comorbid[, score  := rowSums(.SD, na.rm = TRUE), .SDcols = charlson_cols]
    comorbid[, wscore := rowSums(Map(`*`, .SD, weights), na.rm = TRUE),
             .SDcols = charlson_cols]

    comorbid[score == 0,                index := "0"]
    comorbid[score >= 1 & score <= 2,   index := "1-2"]
    comorbid[score >= 3 & score <= 4,   index := "3-4"]
    comorbid[score >= 5,                index := "5+"]

    comorbid[wscore == 0,               windex := "0"]
    comorbid[wscore >= 1 & wscore <= 2, windex := "1-2"]
    comorbid[wscore >= 3 & wscore <= 4, windex := "3-4"]
    comorbid[wscore >= 5,               windex := "5+"]
  }

  tibble::as_tibble(comorbid)
}
