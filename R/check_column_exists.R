#' Check if a Column Exists in a Redshift Table
#'
#' Queries `information_schema.columns` to determine whether a column exists
#' in a given schema and table.
#'
#' @param con A Redshift DBI connection object.
#' @param schema A string specifying the schema name.
#' @param table A string specifying the table name.
#' @param column A string specifying the column name.
#'
#' @return Logical. `TRUE` if the column exists, `FALSE` otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' check_column_exists(con, schema = "public", table = "patients",
#'                     column = "patient_id")
#' }
check_column_exists <- function(con, schema, table, column) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(schema, min.chars = 1)
  checkmate::assert_string(table, min.chars = 1)
  checkmate::assert_string(column, min.chars = 1)

  #--- Build and execute query ---
  sql_query <- glue::glue_sql(
    "SELECT 1
     FROM information_schema.columns
     WHERE table_schema = {schema}
       AND table_name   = {table}
       AND column_name  = {column}
     LIMIT 1",
    .con = con
  )

  #--- Return logical result ---
  nrow(DBI::dbGetQuery(con, sql_query)) > 0
}
