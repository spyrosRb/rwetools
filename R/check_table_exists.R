#' Check if a Table Exists in Redshift
#'
#' Queries `information_schema.tables` to determine whether a given table
#' exists. Optionally scoped to a specific schema.
#'
#' @param con A Redshift DBI connection object.
#' @param table_name A string specifying the table name to check.
#' @param schema_name An optional string specifying the schema name. Default
#'   `NULL` (searches across all schemas).
#'
#' @return Logical. `TRUE` if the table exists, `FALSE` otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' check_table_exists(con, "my_table")
#' check_table_exists(con, "my_table", schema_name = "public")
#' }
check_table_exists <- function(con, table_name, schema_name = NULL) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(table_name, min.chars = 1)
  checkmate::assert_string(schema_name, min.chars = 1, null.ok = TRUE)

  #--- Build query with optional schema filter ---
  schema_condition <- if (!is.null(schema_name)) {
    glue::glue_sql("AND table_schema = {schema_name}", .con = con)
  } else {
    ""
  }

  sql_query <- glue::glue_sql(
    "SELECT 1 FROM information_schema.tables
     WHERE table_name = {table_name}
     {schema_condition}
     LIMIT 1",
    .con = con
  )

  #--- Return logical result ---
  nrow(DBI::dbGetQuery(con, sql_query)) > 0
}
