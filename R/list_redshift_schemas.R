#' List Available Schemas in Redshift
#'
#' Queries the Redshift catalog to retrieve user-defined schemas, with optional
#' filters for OMOP-related schemas and scratch spaces.
#'
#' @param con A Redshift DBI connection object.
#' @param include_omop Logical. If TRUE, includes schemas whose names contain
#'   `"omop"`. Default `TRUE`.
#' @param include_scratch_space Logical. If TRUE, includes schemas whose names
#'   start with `"scr_"`. Default `FALSE`.
#'
#' @return A data frame of schema names matching the specified filters, sorted
#'   alphabetically.
#' @export
#'
#' @examples
#' \dontrun{
#' list_redshift_schemas(con)
#' list_redshift_schemas(con, include_omop = FALSE, include_scratch_space = TRUE)
#' }
list_redshift_schemas <- function(con,
                                  include_omop         = TRUE,
                                  include_scratch_space = FALSE) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_logical(include_omop, len = 1)
  checkmate::assert_logical(include_scratch_space, len = 1)

  #--- Build SQL with optional filters ---
  base_sql <- "
    SELECT nspname AS schema_name
    FROM pg_namespace
    WHERE nspname NOT IN ('information_schema', 'public', 'catalog_history')
      AND nspname NOT ILIKE 'pg%%'
  "

  filters <- character(0)
  if (!include_omop)          filters <- c(filters, "nspname NOT ILIKE '%omop%'")
  if (!include_scratch_space) filters <- c(filters, "nspname NOT ILIKE 'scr_%'")

  if (length(filters) > 0) {
    base_sql <- paste(base_sql, "AND", paste(filters, collapse = " AND "))
  }

  #--- Execute and return ---
  result <- DBI::dbGetQuery(con, base_sql) |>
    dplyr::arrange(schema_name)

  cli::cli_alert_success("Retrieved {.strong {nrow(result)}} schema(s).")
  result
}
