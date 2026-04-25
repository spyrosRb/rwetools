#' List Schema-Level Permissions for a Redshift User
#'
#' Checks whether a given user has `USAGE` and `CREATE` privileges on a
#' specified Redshift schema.
#'
#' @param con A Redshift DBI connection object.
#' @param username A string specifying the Redshift username.
#' @param schema_name A string specifying the schema name.
#'
#' @return A tibble with columns: `schema`, `permission_type`,
#'   `has_permission`.
#' @export
#'
#' @examples
#' \dontrun{
#' list_schema_permissions(con, username = "jsmith",
#'                         schema_name = "my_schema")
#' }
list_schema_permissions <- function(con, username, schema_name) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(username, min.chars = 1)
  checkmate::assert_string(schema_name, min.chars = 1)

  #--- Build and execute query ---
  sql_query <- glue::glue(
    "SELECT
       '{username}' AS username,
       schemaname AS schema,
       has_schema_privilege('{username}', schemaname, 'create') AS create_schema_permission,
       has_schema_privilege('{username}', schemaname, 'usage')  AS use_schema_permission
     FROM pg_tables
     WHERE schemaname = '{schema_name}'"
  )

  result <- DBI::dbGetQuery(con, sql_query)

  #--- Pivot to long format and return ---
  result |>
    tidyr::pivot_longer(
      cols      = dplyr::ends_with("_permission"),
      names_to  = "permission_type",
      values_to = "has_permission"
    ) |>
    dplyr::select(-username) |>
    dplyr::distinct() |>
    tibble::as_tibble()
}
