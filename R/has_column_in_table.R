#' Check if a Column Exists in a Redshift Table
#'
#' A convenience wrapper around [check_column_exists()] using the argument
#' order expected by legacy callers (`table` before `schema`).
#'
#' @param con A Redshift DBI connection object.
#' @param table A string specifying the table name.
#' @param schema A string specifying the schema name.
#' @param column A string specifying the column name.
#'
#' @return Logical. `TRUE` if the column exists, `FALSE` otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' has_column_in_table(con, table = "patients", schema = "public",
#'                     column = "patient_id")
#' }
has_column_in_table <- function(con, table, schema, column) {
  check_column_exists(con, schema = schema, table = table, column = column)
}
