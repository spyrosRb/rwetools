#' Retrieve Files from an S3 Bucket
#'
#' Downloads one or more files from an S3 bucket to a local directory using
#' a connection object. If an object key already carries a file extension it
#' is used as-is; otherwise `file_type` is appended.
#'
#' @param bucket_name A string specifying the S3 bucket name.
#' @param object_keys A character vector of object keys (file names) to
#'   download.
#' @param target_path A string specifying the path prefix within the bucket.
#' @param local_path A string specifying the local directory to save files
#'   into. Must exist.
#' @param file_type A string specifying the default file extension to append
#'   when an object key has none. Default `"sas7bdat"`.
#' @param con A connection object with a `download_file()` method.
#' @param verbose Logical. If `TRUE` (default), prints progress messages.
#'
#' @return `invisible(NULL)`. Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' retrieve_s3_files(
#'   bucket_name = "my-bucket",
#'   target_path = "2024/data",
#'   object_keys = c("patients.sas7bdat", "visits"),
#'   local_path  = "./output",
#'   con         = s3
#' )
#' }
retrieve_s3_files <- function(bucket_name,
                              object_keys,
                              target_path,
                              local_path,
                              file_type = "sas7bdat",
                              con,
                              verbose   = TRUE) {

  #--- Argument checks ---
  checkmate::assert_string(bucket_name, min.chars = 1)
  checkmate::assert_character(object_keys, min.chars = 1, any.missing = FALSE)
  checkmate::assert_string(target_path, min.chars = 1)
  checkmate::assert_string(local_path, min.chars = 1)
  checkmate::assert_string(file_type, min.chars = 1)
  checkmate::assert_logical(verbose, len = 1)

  if (!dir.exists(local_path)) {
    cli::cli_abort("Local directory {.path {local_path}} does not exist.")
  }

  #--- Download each file ---
  for (object_key in object_keys) {

    existing_ext <- tools::file_ext(object_key)
    file_name    <- if (nzchar(existing_ext)) {
      object_key
    } else {
      paste0(object_key, ".", file_type)
    }

    s3_path <- file.path(bucket_name, target_path, file_name)

    if (verbose) cli::cli_alert_info("Downloading {.file {file_name}}...")

    con$download_file(remote_path = s3_path, local_path = local_path)

    if (verbose) cli::cli_alert_success("Saved to {.path {file.path(local_path, file_name)}}")
  }

  invisible(NULL)
}
