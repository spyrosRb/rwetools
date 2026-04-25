#' Clean the R Environment
#'
#' Removes all data frames from the specified environment, optionally
#' excluding named objects.
#'
#' @param env An environment from which to remove data frames. Default
#'   `.GlobalEnv`.
#' @param exclude A character vector of data frame names to keep. Default
#'   `character()`.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of removing
#'   objects from `env`.
#' @export
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(x = 1)
#' df2 <- data.frame(y = 2)
#' df3 <- data.frame(z = 3)
#'
#' # Remove all data frames
#' clean_environment()
#'
#' # Keep df1
#' clean_environment(exclude = "df1")
#' }
clean_environment <- function(env = .GlobalEnv, exclude = character()) {

  #--- Argument checks ---
  checkmate::assert_environment(env)
  checkmate::assert_character(exclude)

  #--- Identify data frames to remove ---
  all_objs  <- ls(envir = env)
  df_objs   <- all_objs[vapply(all_objs, function(x) is.data.frame(get(x, envir = env)), logical(1))]
  to_remove <- setdiff(df_objs, exclude)

  #--- Remove and report ---
  if (length(to_remove) > 0) {
    rm(list = to_remove, envir = env)
    cli::cli_alert_success("Removed {length(to_remove)} object{?s}:")
    cli::cat_bullet(to_remove, bullet = "bullet")
  } else {
    cli::cli_alert_info("No objects to remove.")
  }

  invisible(NULL)
}
