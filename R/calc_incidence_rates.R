# WORK IN PROGRESS — not yet ready for export
# Uncomment and restore #' roxygen tags when complete.

# calc_incidence_rates <- function(data,
#                                   event_var,
#                                   person_time_var,
#                                   group_vars      = NULL,
#                                   patient_id_var  = NULL,
#                                   person_time_unit = c("days", "years"),
#                                   rate_scale      = 1000,
#                                   digits          = 2) {
#
#   person_time_unit <- match.arg(person_time_unit)
#
#   checkmate::assert_data_frame(data, min.rows = 1)
#   checkmate::assert_string(event_var)
#   checkmate::assert_string(person_time_var)
#   checkmate::assert_character(group_vars, min.len = 1, null.ok = TRUE)
#   checkmate::assert_string(patient_id_var, null.ok = TRUE)
#   checkmate::assert_number(rate_scale, lower = 1)
#   checkmate::assert_count(digits)
#
#   required_cols <- c(event_var, person_time_var, group_vars, patient_id_var)
#   missing_cols  <- setdiff(required_cols, names(data))
#   if (length(missing_cols) > 0) {
#     cli::cli_abort("Column{?s} not found in data: {.val {missing_cols}}")
#   }
#
#   pt_divisor <- if (person_time_unit == "days") 365.25 else 1
#
#   grouped <- if (!is.null(group_vars)) {
#     dplyr::group_by(data, dplyr::across(dplyr::all_of(group_vars)))
#   } else {
#     data
#   }
#
#   sum_exprs <- list(
#     n_events     = rlang::expr(sum(as.numeric(.data[[!!event_var]]),    na.rm = TRUE)),
#     person_years = rlang::expr(sum(.data[[!!person_time_var]], na.rm = TRUE) / !!pt_divisor)
#   )
#   if (!is.null(patient_id_var)) {
#     sum_exprs$n_patients <- rlang::expr(dplyr::n_distinct(.data[[!!patient_id_var]]))
#   }
#
#   out <- dplyr::summarize(grouped, !!!sum_exprs, .groups = "drop")
#
#   out <- dplyr::mutate(
#     out,
#     ir          = round(n_events / person_years * rate_scale, digits),
#     ir_lower_95 = round(
#       qchisq(0.025, 2 * n_events)       / (2 * person_years) * rate_scale, digits
#     ),
#     ir_upper_95 = round(
#       qchisq(0.975, 2 * (n_events + 1)) / (2 * person_years) * rate_scale, digits
#     ),
#     ir_string = paste0(ir, " (", ir_lower_95, ", ", ir_upper_95, ")")
#   )
#
#   if (!is.null(patient_id_var)) {
#     binom_cis <- mapply(
#       function(e, n) {
#         ci <- stats::binom.test(as.integer(round(e)), as.integer(round(n)))$conf.int * 100
#         c(lower = round(ci[[1]], digits), upper = round(ci[[2]], digits))
#       },
#       out$n_events, out$n_patients,
#       SIMPLIFY = FALSE
#     )
#
#     out <- dplyr::mutate(
#       out,
#       n_events_pct = round(n_events / n_patients * 100, digits),
#       ip           = n_events_pct,
#       ip_lower_95  = sapply(binom_cis, `[[`, "lower"),
#       ip_upper_95  = sapply(binom_cis, `[[`, "upper"),
#       ip_string    = paste0(ip, "% (", ip_lower_95, "%, ", ip_upper_95, "%)")
#     )
#
#     col_order <- c(
#       group_vars,
#       "n_patients", "n_events", "n_events_pct", "person_years",
#       "ir", "ir_lower_95", "ir_upper_95", "ir_string",
#       "ip", "ip_lower_95", "ip_upper_95", "ip_string"
#     )
#   } else {
#     col_order <- c(
#       group_vars,
#       "n_events", "person_years",
#       "ir", "ir_lower_95", "ir_upper_95", "ir_string"
#     )
#   }
#
#   dplyr::select(out, dplyr::all_of(col_order))
# }
