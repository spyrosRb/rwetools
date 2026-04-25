#' Execute a SQL CREATE TABLE Statement
#'
#' Drops the target table if it exists, then executes a `CREATE TABLE AS`
#' statement. Optionally prints execution timing via `tictoc`.
#'
#' @param con A Redshift DBI connection object.
#' @param sql_code A string containing the SQL `SELECT` query used to populate
#'   the table.
#' @param table_type A string specifying the table type prefix, e.g.
#'   `"TEMPORARY"` or `""` for a persistent table.
#' @param table_path A string specifying the fully qualified table name (e.g.
#'   `"schema.table"`).
#' @param verbose Logical. If TRUE, prints execution time after completion.
#'   Default `FALSE`.
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
#'
#' @examples
#' \dontrun{
#' execute_sql_create_table(
#'   con        = con,
#'   sql_code   = "SELECT * FROM schema.source_table WHERE year = 2024",
#'   table_type = "",
#'   table_path = "schema.my_table"
#' )
#' }
execute_sql_create_table <- function(con,
                                     sql_code,
                                     table_type,
                                     table_path,
                                     verbose = FALSE) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(sql_code, min.chars = 1)
  checkmate::assert_string(table_type)
  checkmate::assert_string(table_path, min.chars = 1)
  checkmate::assert_flag(verbose)

  #--- Build CREATE TABLE SQL ---
  sql_query <- glue::glue("CREATE {table_type} TABLE {table_path} AS {sql_code}")

  #--- Execute: drop existing then create ---
  if (verbose) cli::cli_h1("Executing SQL")
  tictoc::tic()

  DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {table_path}"))
  DBI::dbExecute(con, sql_query)

  tictoc::toc(quiet = TRUE, log = TRUE)
  cli::cli_alert_success("Table {.val {table_path}} created successfully.")

  if (verbose) {
    print_execution_time()
    tictoc::tic.clearlog()
  }

  invisible(TRUE)
}
