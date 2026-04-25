#' Generate a Qualified SQL Table Path
#'
#' Constructs a fully qualified table reference (`schema.table`) for use in
#' SQL statements. Returns just the table name when no schema is provided.
#'
#' @param table_name A string specifying the table name.
#' @param table_schema An optional string specifying the schema name. Default
#'   `NULL`.
#'
#' @return A string: `"table_name"` or `"schema.table_name"`.
#' @export
#'
#' @examples
#' generate_sql_table_path("patients", NULL)
#' #> "patients"
#'
#' generate_sql_table_path("patients", "public")
#' #> "public.patients"
generate_sql_table_path <- function(table_name, table_schema = NULL) {

  #--- Argument checks ---
  checkmate::assert_string(table_name, min.chars = 1)
  checkmate::assert_string(table_schema, null.ok = TRUE)

  #--- Build and return table path ---
  if (is.null(table_schema) || trimws(table_schema) == "") {
    table_name
  } else {
    glue::glue("{table_schema}.{table_name}")
  }
}
