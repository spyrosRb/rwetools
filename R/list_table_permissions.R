#' List Table-Level Permissions for a Redshift User
#'
#' Checks SELECT, INSERT, UPDATE, DELETE, and REFERENCES privileges for a
#' given user on all tables in a schema, optionally filtered to one table.
#'
#' @param con A Redshift DBI connection object.
#' @param username A string specifying the Redshift username.
#' @param schema_name A string specifying the schema name.
#' @param table_name An optional string to filter to a specific table. Default
#'   `NULL` (all tables in the schema).
#'
#' @return A tibble with columns: `permission_type`, `has_permission`.
#' @export
#'
#' @examples
#' \dontrun{
#' list_table_permissions(con, username = "jsmith",
#'                        schema_name = "public")
#'
#' list_table_permissions(con, username = "jsmith",
#'                        schema_name = "public",
#'                        table_name  = "patients")
#' }
list_table_permissions <- function(con, username, schema_name, table_name = NULL) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(username, min.chars = 1)
  checkmate::assert_string(schema_name, min.chars = 1)
  checkmate::assert_string(table_name, min.chars = 1, null.ok = TRUE)

  #--- Build WHERE clause ---
  where_clause <- glue::glue("schemaname = '{schema_name}'")
  if (!is.null(table_name)) {
    where_clause <- glue::glue("{where_clause} AND tablename = '{table_name}'")
  }

  #--- Build and execute query ---
  sql_query <- glue::glue("
    SELECT
      '{username}' AS username,
      schemaname || '.' || tablename AS full_table_name,
      has_table_privilege('{username}', schemaname || '.' || tablename, 'select')     AS user_has_select_permission,
      has_table_privilege('{username}', schemaname || '.' || tablename, 'insert')     AS user_has_insert_permission,
      has_table_privilege('{username}', schemaname || '.' || tablename, 'update')     AS user_has_update_permission,
      has_table_privilege('{username}', schemaname || '.' || tablename, 'delete')     AS user_has_delete_permission,
      has_table_privilege('{username}', schemaname || '.' || tablename, 'references') AS user_has_references_permission
    FROM pg_tables
    WHERE {where_clause}
  ")

  result <- DBI::dbGetQuery(con, sql_query)

  #--- Pivot to long format and return ---
  result |>
    tidyr::pivot_longer(
      cols      = dplyr::starts_with("user_has_"),
      names_to  = "permission_type",
      values_to = "has_permission"
    ) |>
    dplyr::select(-full_table_name, -username) |>
    tibble::as_tibble()
}
