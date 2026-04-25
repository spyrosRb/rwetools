#' Retrieve Column Metadata from a Redshift Table
#'
#' Queries `information_schema.columns` to return metadata for columns
#' matching the supplied database, schema, and/or table name filters.
#' All filter arguments are optional — omitting all returns every column
#' accessible to the connection.
#'
#' @param con A Redshift DBI connection object.
#' @param database An optional string specifying the database (catalog) name.
#' @param schema An optional string specifying the schema name.
#' @param table_name An optional string specifying the table name.
#'
#' @return A data frame with columns: `database`, `schema`, `table_name`,
#'   `column_name`, `ordinal_position`, `data_type`, `base_type`,
#'   `max_length`.
#' @export
#'
#' @examples
#' \dontrun{
#' get_column_info(con, schema = "public", table_name = "patients")
#' }
get_column_info <- function(con,
                            database   = NULL,
                            schema     = NULL,
                            table_name = NULL) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(database,   null.ok = TRUE)
  checkmate::assert_string(schema,     null.ok = TRUE)
  checkmate::assert_string(table_name, null.ok = TRUE)

  #--- Build WHERE clause with safely quoted values ---
  filters <- character(0)
  if (!is.null(database))
    filters <- c(filters, paste0("table_catalog = ",
                                 DBI::dbQuoteString(con, database)))
  if (!is.null(schema))
    filters <- c(filters, paste0("table_schema = ",
                                 DBI::dbQuoteString(con, schema)))
  if (!is.null(table_name))
    filters <- c(filters, paste0("table_name = ",
                                 DBI::dbQuoteString(con, table_name)))

  where_clause <- if (length(filters) > 0) {
    paste("WHERE", paste(filters, collapse = " AND "))
  } else {
    ""
  }

  #--- Build and execute query ---
  sql_query <- glue::glue("
    SELECT table_catalog              AS database,
           table_schema               AS schema,
           table_name,
           column_name,
           ordinal_position,
           data_type,
           udt_name                   AS base_type,
           character_maximum_length   AS max_length
    FROM information_schema.columns
    {where_clause}
    ORDER BY ordinal_position
  ")

  DBI::dbGetQuery(con, sql_query)
}
