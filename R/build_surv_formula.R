#' Build a Survival Analysis Formula
#'
#' Constructs a `Surv()` formula for use in Kaplan-Meier or Cox proportional
#' hazards models. When `covariates` is `NULL` a KM intercept-only formula is
#' returned; otherwise covariates are joined with `+` for use in Cox models.
#'
#' @param time A string specifying the survival time variable name.
#' @param status A string specifying the event/censoring status variable name
#'   (typically 0/1).
#' @param covariates An optional character vector of covariate names to include
#'   on the right-hand side. Default `NULL` (intercept-only: `~ 1`).
#'
#' @return A `formula` object of the form `Surv(time, status) ~ covariates`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Kaplan-Meier (no covariates)
#' build_surv_formula(time = "os_time", status = "os_status")
#'
#' # KM stratified by one variable
#' build_surv_formula(time = "os_time", status = "os_status",
#'                    covariates = "treatment")
#'
#' # Cox model with multiple covariates
#' build_surv_formula(time = "os_time", status = "os_status",
#'                    covariates = c("age", "sex", "treatment"))
#' }
build_surv_formula <- function(time, status, covariates = NULL) {

  #--- Argument checks ---
  checkmate::assert_string(time, min.chars = 1)
  checkmate::assert_string(status, min.chars = 1)
  checkmate::assert_character(covariates, min.chars = 1, null.ok = TRUE)

  #--- Build formula components ---
  lhs <- glue::glue("Surv({time}, {status})")
  rhs <- if (is.null(covariates)) "1" else paste(covariates, collapse = " + ")

  #--- Construct and return formula ---
  stats::reformulate(rhs, response = lhs)
}
