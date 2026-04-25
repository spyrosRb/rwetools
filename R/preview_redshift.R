#' Preview a Redshift Table
#'
#' Retrieves the first `n` rows from a Redshift table, analogous to
#' `head()`.
#'
#' @param con A Redshift DBI connection object.
#' @param table A string specifying the table name.
#' @param schema A string specifying the schema name.
#' @param n A positive integer specifying the number of rows to retrieve.
#'   Default `6`.
#'
#' @return A data frame containing the first `n` rows of the table.
#' @export
#'
#' @examples
#' \dontrun{
#' preview_redshift(con, table = "patients", schema = "public")
#' preview_redshift(con, table = "patients", schema = "public", n = 20)
#' }
preview_redshift <- function(con, table, schema, n = 6) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(table, min.chars = 1)
  checkmate::assert_string(schema, min.chars = 1)
  checkmate::assert_count(n, positive = TRUE)

  #--- Build and execute query ---
  sql_query <- glue::glue("SELECT * FROM {schema}.{table} LIMIT {n};")
  DBI::dbGetQuery(con, sql_query)
}
