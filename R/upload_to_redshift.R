#' Upload a Data Frame to a Redshift Scratch Schema
#'
#' Uploads a data frame to a specified table in a Redshift scratch schema
#' using a RocheData connection object.
#'
#' @param con A RocheData Redshift connection object.
#' @param data A data frame containing the data to upload.
#' @param scratch_space A string specifying the target schema name.
#' @param table_name A string specifying the target table name.
#' @param overwrite Logical. If TRUE, replaces an existing table. Default
#'   `TRUE`.
#'
#' @return Invisibly returns the result of the upload call.
#' @export
#'
#' @examples
#' \dontrun{
#' upload_to_redshift(
#'   con          = db,
#'   data         = my_df,
#'   scratch_space = "scr_myproject",
#'   table_name   = "my_table"
#' )
#' }
upload_to_redshift <- function(con, data, scratch_space, table_name, overwrite = TRUE) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_data_frame(data)
  checkmate::assert_string(scratch_space, min.chars = 1)
  checkmate::assert_string(table_name, min.chars = 1)
  checkmate::assert_flag(overwrite)

  #--- Set schema and upload ---
  cli::cli_alert_info("Setting target schema to {.val {scratch_space}}.")
  con$schema <- scratch_space

  cli::cli_alert_info("Uploading {.val {table_name}} to schema {.val {scratch_space}}...")

  result <- data |>
    con$create_table(
      table_name   = table_name,
      write_method = "bulk",
      overwrite    = overwrite
    )

  cli::cli_alert_success("Table {.val {table_name}} uploaded successfully.")
  invisible(result)
}
