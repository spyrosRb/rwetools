#' List Tables in a Redshift Schema
#'
#' Retrieves all tables from a specified Redshift schema along with their
#' column counts and row counts.
#'
#' @param con A Redshift DBI connection object.
#' @param schema A string specifying the schema name.
#'
#' @return A tibble with columns: `table_schema`, `table_name`,
#'   `total_columns`, `total_rows`.
#' @export
#'
#' @examples
#' \dontrun{
#' list_redshift_tables(con, schema = "public")
#' }
list_redshift_tables <- function(con, schema) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(schema, min.chars = 1)

  #--- Fetch column counts per table ---
  col_counts <- DBI::dbGetQuery(con, glue::glue_sql(
    "SELECT table_schema, table_name, COUNT(*) AS total_columns
     FROM information_schema.columns
     WHERE table_schema = {schema}
     GROUP BY table_schema, table_name
     ORDER BY table_schema, table_name",
    .con = con
  ))

  if (nrow(col_counts) == 0) {
    cli::cli_alert_warning("No tables found in schema {.val {schema}}.")
    return(tibble::tibble())
  }

  #--- Fetch row counts per table ---
  cli::cli_progress_bar("Querying row counts", total = nrow(col_counts))
  row_counts <- list()

  for (tbl in col_counts$table_name) {
    row_counts[[tbl]] <- tryCatch({
      DBI::dbGetQuery(con, glue::glue_sql(
        "SELECT COUNT(*) AS total_rows, {tbl} AS table_name FROM {`schema`}.{`tbl`}",
        .con = con
      ))
    }, error = function(e) {
      cli::cli_alert_danger("Failed to count rows for {.val {tbl}}: {.emph {e$message}}")
      data.frame(total_rows = NA_integer_, table_name = tbl)
    })
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  #--- Combine and return ---
  col_counts |>
    dplyr::left_join(dplyr::bind_rows(row_counts), by = "table_name") |>
    tibble::as_tibble()
}
