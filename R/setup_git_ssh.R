#' Set Up Git SSH Authentication
#'
#' Ensures Git username and email are configured, then generates a new SSH key
#' pair of the requested type and registers it with the SSH agent. If a key of
#' that type already exists it is replaced. The platform (GitHub vs GitLab) is
#' inferred from the email domain: addresses containing `"roche"` are treated
#' as GitLab; all others as GitHub.
#'
#' @param key_type A string specifying the SSH key type: `"ed25519"` (default)
#'   or `"rsa"`.
#'
#' @return `invisible(NULL)`. Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' setup_git_ssh()
#' setup_git_ssh(key_type = "rsa")
#' }
setup_git_ssh <- function(key_type = "ed25519") {

  #--- Argument checks ---
  checkmate::assert_string(key_type, pattern = "^(ed25519|rsa)$")

  #--- Retrieve Git credentials (coerce empty results to "") ---
  username <- system("git config --global user.name",  intern = TRUE)
  email    <- system("git config --global user.email", intern = TRUE)
  if (length(username) == 0) username <- ""
  if (length(email)    == 0) email    <- ""

  #--- Prompt for missing credentials ---
  if (!nzchar(username) | !nzchar(email)) {
    cli::cli_alert_warning("Git username or email is missing. Configuring Git profile...")
    configure_git_profile(username, email)
    username <- system("git config --global user.name",  intern = TRUE)
    email    <- system("git config --global user.email", intern = TRUE)
  }

  #--- Infer platform from email domain ---
  platform <- if (grepl("roche", tolower(email), fixed = TRUE)) "GitLab" else "GitHub"

  #--- Ensure .ssh directory exists ---
  home_path     <- fs::path_home()
  ssh_directory <- fs::path(home_path, ".ssh")
  ssh_key_path  <- fs::path(ssh_directory, paste0("id_", key_type))

  if (!fs::dir_exists(ssh_directory)) {
    fs::dir_create(ssh_directory)
    cli::cli_alert_success("Created .ssh directory at {.path {ssh_directory}}")
  }

  #--- Remove existing key if present ---
  pub_path <- paste0(ssh_key_path, ".pub")
  if (fs::file_exists(ssh_key_path) | fs::file_exists(pub_path)) {
    file.remove(as.character(ssh_key_path), pub_path)
    cli::cli_alert_warning("Existing {key_type} key removed from {.path {ssh_directory}}")
  }

  #--- Generate new key ---
  generate_ssh_key(key_type = key_type, email = email,
                   ssh_path = as.character(ssh_key_path), platform = platform)

  invisible(NULL)
}
