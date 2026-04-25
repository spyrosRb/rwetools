#' Assert One Row per Patient in a Database Table
#'
#' Queries a Redshift table to verify that each patient has exactly one row,
#' by comparing total row count to the count of distinct patient IDs. Results
#' are reported to the console via `cli`.
#'
#' @param con A Redshift DBI connection object.
#' @param table_name A string specifying the table name.
#' @param schema An optional string specifying the schema name. Default `NULL`.
#' @param patient_id A string specifying the patient ID column name. Default
#'   `"patient_id"`.
#'
#' @return `invisible(NULL)`. Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' assert_unique_patients(con, table_name = "patients", schema = "public")
#' }
assert_unique_patients <- function(con,
                                   table_name,
                                   schema     = NULL,
                                   patient_id = "patient_id") {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(table_name, min.chars = 1)
  checkmate::assert_string(schema, null.ok = TRUE)
  checkmate::assert_string(patient_id, min.chars = 1)

  #--- Build qualified table reference ---
  table_path <- generate_sql_table_path(table_name = table_name,
                                        table_schema = schema)

  #--- Query total rows and distinct patient count ---
  sql_query <- glue::glue(
    "SELECT COUNT(*) AS total_rows,
            COUNT(DISTINCT {patient_id}) AS unique_patients
     FROM {table_path}"
  )

  counts          <- DBI::dbGetQuery(con, sql_query)
  total_rows      <- counts$total_rows
  unique_patients <- counts$unique_patients

  #--- Report results ---
  cli::cli_h1("Patient Uniqueness Check")
  cli::cli_text("Table: {.strong {table_path}}")
  cli::cli_alert_info("Total rows: {.val {total_rows}}")
  cli::cli_alert_info("Unique patients: {.val {unique_patients}}")

  if (total_rows == unique_patients) {
    cli::cli_alert_success("One row per patient confirmed.")
  } else {
    cli::cli_alert_danger(
      "Mismatch detected: {total_rows - unique_patients} duplicate row(s) found."
    )
  }

  invisible(NULL)
}
