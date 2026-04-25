#' Drop a Redshift Table
#'
#' Safely drops a table from Redshift using `DROP TABLE IF EXISTS`, with
#' optional CLI feedback.
#'
#' @param con A Redshift DBI connection object.
#' @param table_path A string specifying the table to drop, optionally
#'   schema-qualified (e.g. `"schema.table_name"`).
#' @param verbose Logical. If TRUE, prints status messages. Default `TRUE`.
#'
#' @return Invisibly returns `TRUE` on success, `FALSE` on error.
#' @export
#'
#' @examples
#' \dontrun{
#' drop_redshift_table(con, "schema.temp_table")
#' }
drop_redshift_table <- function(con, table_path, verbose = TRUE) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(table_path, min.chars = 1)
  checkmate::assert_flag(verbose)

  #--- Execute DROP ---
  result <- tryCatch({
    DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {table_path}"))
    if (verbose) cli::cli_alert_success("Table {.val {table_path}} dropped successfully.")
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Failed to drop {.val {table_path}}: {e$message}")
    FALSE
  })

  invisible(result)
}
