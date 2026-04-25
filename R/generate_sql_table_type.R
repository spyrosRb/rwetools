#' Determine Redshift SQL Table Type
#'
#' Returns the SQL keyword for the table type based on whether a schema is
#' provided. Used when dynamically constructing `CREATE TABLE` statements.
#'
#' @param table_schema An optional string specifying the schema name. If `NULL`
#'   or blank, the table is treated as temporary. Default `NULL`.
#'
#' @return `"TEMPORARY"` when no schema is given; `""` otherwise.
#' @export
#'
#' @examples
#' generate_sql_table_type(NULL)      # "TEMPORARY"
#' generate_sql_table_type("")        # "TEMPORARY"
#' generate_sql_table_type("public")  # ""
generate_sql_table_type <- function(table_schema = NULL) {

  #--- Argument checks ---
  checkmate::assert_string(table_schema, null.ok = TRUE)

  #--- Return table type keyword ---
  if (is.null(table_schema) || trimws(table_schema) == "") {
    "TEMPORARY"
  } else {
    ""
  }
}
