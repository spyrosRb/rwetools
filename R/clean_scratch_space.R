#' Drop All Tables from a Scratch Schema
#'
#' Retrieves all table names from a specified database schema and drops them
#' one by one, except for those listed in `keep`. CLI feedback is provided
#' via a progress bar and a final summary.
#'
#' @param con A DBI database connection object.
#' @param scratch_space A string specifying the schema name from which to drop
#'   tables.
#' @param keep A character vector of table names to exclude from deletion.
#'   Default `NULL` (drop all).
#'
#' @return Invisibly returns a character vector of successfully dropped table
#'   names.
#' @export
#'
#' @examples
#' \dontrun{
#' # Drop all tables in schema
#' clean_scratch_space(con = con, scratch_space = "scratch_schema")
#'
#' # Keep specific tables
#' clean_scratch_space(
#'   con           = con,
#'   scratch_space = "scratch_schema",
#'   keep          = c("important_log", "keep_me")
#' )
#' }
clean_scratch_space <- function(con, scratch_space, keep = NULL) {

  #--- Argument checks ---
  checkmate::assert_class(con, "DBIConnection")
  checkmate::assert_string(scratch_space, min.chars = 1)
  checkmate::assert_character(keep, null.ok = TRUE)

  #--- Fetch tables from scratch schema ---
  cli::cli_alert_info("Fetching tables from scratch space {.val {scratch_space}}...")

  sql_query <- glue::glue_sql(
    "SELECT table_name
     FROM information_schema.tables
     WHERE table_schema = {scratch_space}
     ORDER BY table_name",
    .con = con
  )

  all_tables <- DBI::dbGetQuery(con, sql_query)$table_name

  if (length(all_tables) == 0) {
    cli::cli_alert_info("No tables found in schema {.val {scratch_space}}.")
    return(invisible(character(0)))
  }

  #--- Determine tables to drop ---
  if (!is.null(keep)) {
    skipped <- setdiff(keep, all_tables)
    to_drop <- setdiff(all_tables, keep)

    if (length(skipped) > 0) {
      cli::cli_alert_warning("Kept tables not found in schema: {.val {skipped}}")
    }

    cli::cli_alert_info("Dropping {length(to_drop)} of {length(all_tables)} tables.")
  } else {
    to_drop <- all_tables
    cli::cli_alert_info("Dropping all {length(to_drop)} tables in {.val {scratch_space}}.")
  }

  #--- Drop tables with progress reporting ---
  cli::cli_progress_bar("Dropping tables", total = length(to_drop))
  dropped <- character(0)

  for (table in to_drop) {
    drop_sql <- glue::glue_sql(
      "DROP TABLE IF EXISTS {`scratch_space`}.{`table`}",
      .con = con
    )

    tryCatch({
      DBI::dbExecute(con, drop_sql)
      dropped <- c(dropped, table)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to drop {.val {table}}: {.emph {e$message}}")
    })

    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success("Done. Dropped {length(dropped)} of {length(to_drop)} table{?s}.")

  invisible(dropped)
}
