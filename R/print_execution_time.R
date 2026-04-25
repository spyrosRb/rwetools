#' Print Last Execution Time from tictoc Log
#'
#' Retrieves the most recent timing entry from `tictoc::tic.log()` and prints
#' it with a green tick bullet via `cli`.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of printing.
#' @export
#'
#' @examples
#' \dontrun{
#' tictoc::tic()
#' Sys.sleep(1)
#' tictoc::toc(log = TRUE)
#' print_execution_time()
#' }
print_execution_time <- function() {

  cli::cat_bullet(
    glue::glue(cli::col_green(
      "Execution time: {cli::col_green(tictoc::tic.log(format = TRUE))}"
    )),
    bullet     = "tick",
    bullet_col = "green"
  )

  invisible(NULL)
}
