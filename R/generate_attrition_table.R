#' Generate an attrition table
#'
#' Creates a summary attrition table using binary inclusion/exclusion flags.
#'
#' @param attrition_table A data.frame or data.table with patient-level flags (e.g., `critfn1`, `critfn2`, etc.).
#' @param person_id A string for the column name that uniquely identifies patients. Default is "patient_number".
#' @param criterion_prefix A prefix used to identify criterion columns (default is "critfn").
#' @param attrition_labels A data.frame or data.table with labels for each criterion (must have a column `criteria`).
#'
#' @return A `data.table` formatted for display.
#' @importFrom data.table setDT melt rollup setnames shift fifelse setcolorder :=
#' @importFrom checkmate assert_data_frame assert_string
#' @examples
#' \dontrun{
#' attrition_table <- generate_attrition_table(
#'   attrition_table = semaglutide_cwm_attrition,
#'   person_id = "patient_id_synth",
#'   criterion_prefix = "critfn",
#'   attrition_labels = attrition_labels
#' )
#' }
#' @export
generate_attrition_table <- function(attrition_table,
                                     person_id = "patient_number",
                                     criterion_prefix = "critfn",
                                     attrition_labels) {

  col1 <- col2 <- col3 <- col4 <- critnum <- total <- .N <- .SD <- grouping <- NULL

  # Validation
  checkmate::assert_data_frame(attrition_table, min.rows = 1)
  checkmate::assert_data_frame(attrition_labels, min.rows = 1)
  checkmate::assert_string(person_id)
  checkmate::assert_string(criterion_prefix)


  # Covert attrition_table and attrition_labels to data.table
  data.table::setDT(attrition_table)
  data.table::setDT(attrition_labels)

  attrition_labels[, critnum := seq_len(.N)]
  id_vars <- person_id

  for (i in seq_along(grep(criterion_prefix, names(attrition_table), value = TRUE))) {
    cols <- paste0(criterion_prefix, 1:i)
    col <- paste0("criterion", i)
    attrition_table[, (col) := ifelse(Reduce(`+`, .SD) == length(cols), 1, 0), .SDcols = cols]
  }

  attrition_table <- data.table::melt(
    attrition_table,
    id.vars = id_vars,
    variable.name = "critnum",
    measure.vars = grep("^criterion\\d+$", names(attrition_table), value = TRUE)
  )

  attrition_table <- data.table::rollup(
    attrition_table,
    j = lapply(.SD, sum),
    by = "critnum",
    id = TRUE,
    .SDcols = "value"
  )

  data.table::setnames(attrition_table, "value", "col1")
  attrition_table[grouping == 0, `:=`(
    total = max(col1),
    col3 = data.table::shift(col1, 1, type = "lag") - col1
  )]

  attrition_table[, `:=`(
    col2 = col1 / total,
    col4 = col3 / total
  )]

  attrition_table[, critnum := as.numeric(gsub("[^\\d]+", "", critnum, perl = TRUE))]
  attrition_table <- merge(attrition_table, attrition_labels, by = "critnum")

  attrition_table[, c("grouping", "total") := NULL]
  attrition_table[, col1 := formatC(col1, format = "d", big.mark = ",")]
  attrition_table[, col2 := data.table::fifelse(
    is.na(col2), "0.0%", scales::label_percent(accuracy = 0.1)(col2)
  )]
  attrition_table[, col3 := data.table::fifelse(
    is.na(col3), "0", formatC(col3, format = "d", big.mark = ",")
  )]
  attrition_table[, col4 := data.table::fifelse(
    is.na(col4), "0.0%", scales::label_percent(accuracy = 0.1)(col4)
  )]

  data.table::setcolorder(attrition_table, c("critnum", "criteria", "col1", "col2", "col3", "col4"))
  return(attrition_table)
}
