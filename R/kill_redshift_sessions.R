#' Find and kill active Redshift sessions
#'
#' Queries `STV_SESSIONS` for active sessions (excluding the current one),
#' displays them, and optionally terminates them using `PG_TERMINATE_BACKEND()`.
#'
#' @param con A Redshift database connection object (e.g. from `DBI::dbConnect()`).
#' @param pids Integer vector or NULL. Process IDs to kill. If NULL (default),
#'   all sessions other than the current one are targeted. Use `"list"` to
#'   only display sessions without killing any.
#' @param user Character or NULL. Filter sessions to a specific username before
#'   killing. Ignored when `pids` is supplied.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return A data frame of the sessions that were targeted for termination
#'   (invisibly), with an added `terminated` column indicating success.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RPostgres::Redshift(), ...)
#'
#' # List active sessions without killing
#' kill_redshift_sessions(con, pids = "list")
#'
#' # Kill all other sessions
#' kill_redshift_sessions(con)
#'
#' # Kill specific PIDs
#' kill_redshift_sessions(con, pids = c(12345L, 67890L))
#'
#' # Kill all sessions for a specific user
#' kill_redshift_sessions(con, user = "analyst1")
#' }
#'
#' @export
kill_redshift_sessions <- function(con,
                                   pids = NULL,
                                   user = NULL,
                                   verbose = TRUE) {

  # --- Check function arguments ---
  checkmate::assert_flag(verbose)
  checkmate::assert_character(user, len = 1, null.ok = TRUE)

  list_only <- identical(pids, "list")

  if (!list_only) {
    checkmate::assert(
      checkmate::check_null(pids),
      checkmate::check_integerish(pids, min.len = 1)
    )
  }

  # --- Query active sessions (excluding current session) ---
  if (verbose) cli::cli_alert_info("Querying active Redshift sessions...")

  sessions <- DBI::dbGetQuery(con, "
    SELECT
      s.process      AS pid,
      s.user_name    AS username,
      s.db_name      AS database,
      s.starttime    AS session_start,
      q.text         AS query_text
    FROM stv_sessions s
    LEFT JOIN (
      SELECT pid, MAX(starttime) AS last_start
      FROM stv_recents
      GROUP BY pid
    ) lr ON s.process = lr.pid
    LEFT JOIN stv_recents q
      ON s.process = q.pid AND q.starttime = lr.last_start
    WHERE s.process <> pg_backend_pid()
    ORDER BY s.starttime
  ")

  if (nrow(sessions) == 0) {
    if (verbose) cli::cli_alert_success("No other active sessions found.")
    return(invisible(sessions))
  }

  if (verbose) {
    cli::cli_alert_info("Found {nrow(sessions)} active session{?s}:")
    cli::cli_rule()
    print(sessions[, c("pid", "username", "database", "session_start")])
    cli::cli_rule()
  }

  # --- Return early if list-only mode ---
  if (list_only) {
    return(invisible(sessions))
  }

  # --- Determine target sessions ---
  targets <- sessions

  if (!is.null(pids)) {
    targets <- dplyr::filter(targets, pid %in% as.integer(pids))

    missing_pids <- setdiff(as.integer(pids), targets$pid)
    if (length(missing_pids) > 0) {
      cli::cli_warn("PID{?s} not found in active sessions: {.val {missing_pids}}")
    }
  } else if (!is.null(user)) {
    targets <- dplyr::filter(targets, username == user)
  }

  if (nrow(targets) == 0) {
    cli::cli_alert_warning("No matching sessions to terminate.")
    return(invisible(targets))
  }

  if (verbose) {
    cli::cli_alert_info(
      "Terminating {nrow(targets)} session{?s} (PID{?s}: {.val {targets$pid}})..."
    )
  }

  # --- Terminate each session ---
  targets$terminated <- vapply(targets$pid, function(pid) {
    result <- DBI::dbGetQuery(
      con,
      paste0("SELECT pg_terminate_backend(", pid, ") AS ok")
    )
    isTRUE(result$ok)
  }, logical(1))

  n_ok   <- sum(targets$terminated)
  n_fail <- sum(!targets$terminated)

  if (verbose) {
    if (n_ok  > 0) cli::cli_alert_success("{n_ok} session{?s} terminated successfully.")
    if (n_fail > 0) cli::cli_alert_warning("{n_fail} session{?s} could not be terminated.")
  }

  invisible(targets)
}
