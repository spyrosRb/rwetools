#' Set Up a New RWE Project Directory
#'
#' Copies standard template files (Dockerfile, gitlab-ci.yml, Snakefile,
#' index.qmd, custom.scss, roche_logo.png) from the package's bundled
#' templates into the target directory, and optionally creates the standard
#' folder structure used by the RWD team.
#'
#' @param dir A string specifying the project root path. Defaults to the
#'   current working directory.
#' @param create_dirs Logical. If `TRUE` (default), creates standard
#'   subdirectories: `analysis/`, `output/`, `temp/`, `R/`.
#' @param overwrite Logical. If `FALSE` (default), existing files are skipped
#'   with a warning. Set `TRUE` to replace them.
#'
#' @return Invisibly returns a character vector of successfully copied file
#'   paths.
#' @export
#'
#' @examples
#' \dontrun{
#' setup_rwe_project()
#' setup_rwe_project(dir = "~/projects/my_study", overwrite = TRUE)
#' }
setup_rwe_project <- function(dir         = getwd(),
                              create_dirs = TRUE,
                              overwrite   = FALSE) {

  #--- Argument checks ---
  checkmate::assert_string(dir)
  checkmate::assert_flag(create_dirs)
  checkmate::assert_flag(overwrite)

  templates_dir <- system.file("templates", package = "rwetools", mustWork = TRUE)

  templates <- c(
    "Dockerfile",
    "gitlab-ci.yml",
    "Snakefile",
    "index.qmd",
    "custom.scss",
    "roche_logo.png"
  )

  #--- Create project root if needed ---
  dir <- normalizePath(dir, mustWork = FALSE)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cli::cli_alert_success("Created project directory: {.path {dir}}")
  }

  #--- Create standard subdirectories ---
  if (create_dirs) {
    for (subdir in file.path(dir, c("analysis", "output", "temp", "R"))) {
      if (!dir.exists(subdir)) {
        dir.create(subdir, recursive = TRUE)
        cli::cli_alert_success("Created {.path {basename(subdir)}/}")
      }
    }
  }

  #--- Copy template files ---
  cli::cli_h1("Copying template files")
  copied <- character(0)

  for (template in templates) {
    src  <- file.path(templates_dir, template)
    dest <- file.path(dir, template)

    if (!file.exists(src)) {
      cli::cli_alert_warning("Template not found, skipping: {.file {template}}")
      next
    }

    if (file.exists(dest) & !overwrite) {
      cli::cli_alert_warning(
        "Skipping {.file {template}} — already exists (use {.code overwrite = TRUE} to replace)"
      )
      next
    }

    if (file.copy(from = src, to = dest, overwrite = overwrite)) {
      cli::cli_alert_success("Copied {.file {template}}")
      copied <- c(copied, dest)
    } else {
      cli::cli_alert_danger("Failed to copy {.file {template}}")
    }
  }

  cli::cli_rule()
  cli::cli_alert_info(
    "{length(copied)} of {length(templates)} template file{?s} copied to {.path {dir}}"
  )

  invisible(copied)
}
