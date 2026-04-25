#' Generate and Register an SSH Key
#'
#' Generates an SSH key pair of the specified type, adds the private key to
#' the SSH agent, prints the public key to the console, and opens the
#' platform's SSH key settings page in the default browser.
#'
#' @param key_type A string specifying the SSH key type: `"ed25519"` or
#'   `"rsa"`.
#' @param email A string specifying the email address to embed in the key
#'   comment.
#' @param ssh_path A string specifying the full file path for the new key
#'   (without `.pub` extension).
#' @param platform A string: `"GitHub"` or `"GitLab"`.
#'
#' @return `invisible(NULL)`. Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' generate_ssh_key("ed25519", "user@example.com",
#'                  "~/.ssh/id_ed25519", "GitHub")
#' }
generate_ssh_key <- function(key_type, email, ssh_path, platform) {

  #--- Argument checks ---
  checkmate::assert_string(key_type, pattern = "^(ed25519|rsa)$")
  checkmate::assert_string(email, pattern = "^\\S+@\\S+\\.\\S+$")
  checkmate::assert_string(ssh_path, min.chars = 1)
  checkmate::assert_choice(platform, c("GitHub", "GitLab"))

  #--- Generate SSH key pair ---
  cli::cli_alert_info("Generating {key_type} SSH key...")
  system(paste("ssh-keygen -t", key_type, "-C", shQuote(email),
               "-f", ssh_path, "-N ''"))

  #--- Start SSH agent and register key ---
  cli::cli_alert_info("Initialising SSH agent...")
  system("eval $(ssh-agent -s)")

  cli::cli_alert_info("Adding SSH key to agent...")
  system(paste("ssh-add", ssh_path))
  cli::cli_alert_success("SSH key added to agent.")

  #--- Display public key ---
  cli::cli_text("--- Begin SSH Public Key ---")
  system(paste("cat", paste0(ssh_path, ".pub")))
  cli::cli_text("--- End SSH Public Key ---")

  #--- Open platform settings page ---
  platform_url <- switch(platform,
    "GitHub" = "https://github.com/settings/ssh/new",
    "GitLab" = "https://code.roche.com/-/user_settings/ssh_keys"
  )

  cli::cli_alert_info("Opening {platform} SSH settings page...")
  utils::browseURL(platform_url)

  invisible(NULL)
}
