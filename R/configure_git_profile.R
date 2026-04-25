#' Configure Git Username and Email
#'
#' Checks whether a Git username and email are already set. When either is
#' blank, prompts the user via RStudio and writes the value to the global Git
#' configuration.
#'
#' @param username A string specifying the Git username. Pass an empty string
#'   `""` to trigger an interactive prompt.
#' @param email A string specifying the Git email address. Pass an empty string
#'   `""` to trigger an interactive prompt.
#'
#' @return `invisible(NULL)`. Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' configure_git_profile(username = "", email = "")
#' configure_git_profile(username = "jdoe", email = "jdoe@example.com")
#' }
configure_git_profile <- function(username, email) {

  #--- Argument checks ---
  checkmate::assert_string(username)
  checkmate::assert_string(email, pattern = "^\\S+@\\S+\\.\\S+$|^$")

  #--- Configure username ---
  if (!nzchar(username)) {
    cli::cli_alert_info("Git username is not set. Prompting for input...")
    username <- rstudioapi::showPrompt(
      title   = "Git username configuration",
      message = "Git username is not set. Please provide a username."
    )
    system(glue::glue("git config --global user.name '{username}'"))
    cli::cli_alert_success("Git username set successfully.")
  } else {
    cli::cli_alert_info("Git username is already set to: {username}")
  }

  #--- Configure email ---
  if (!nzchar(email)) {
    cli::cli_alert_info("Git email is not set. Prompting for input...")
    email_prompt <- rstudioapi::showPrompt(
      title   = "Git email configuration",
      message = "Git email is not set. Please provide an email."
    )
    system(glue::glue("git config --global user.email '{email_prompt}'"))
    cli::cli_alert_success("Git email set successfully.")
  } else {
    cli::cli_alert_info("Git email is already set to: {email}")
  }

  invisible(NULL)
}
