#' Print Redshift Table Statistics
#'
#' Queries a Redshift table for record counts and unique patient counts.
#' Optionally extends the output with records-per-patient statistics, date
#' range, and yearly record counts.
#'
#' @param con A Redshift DBI connection object.
#' @param table_name A string specifying the table name (may include schema,
#'   e.g. `"schema.table"`).
#' @param patient_id A string specifying the patient identifier column.
#'   Default `"patient_id_synth"`.
#' @param date_column A string specifying the date column used for extended
#'   statistics. Default `"from_dt"`.
#' @param extended Logical. If TRUE, also queries records-per-patient
#'   distribution, date range, and yearly counts. Default `FALSE`.
#'
#' @return Invisibly returns a named list of statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' print_table_stats(con, "schema.my_table")
#' print_table_stats(con, "schema.my_table", extended = TRUE)
#' }
print_table_stats <- function(con,
                              table_name,
                              patient_id  = "patient_id_synth",
                              date_column = "from_dt",
                              extended    = FALSE) {

  #--- Argument checks ---
  checkmate::assert_class(con, "RedshiftConnection")
  checkmate::assert_string(table_name, min.chars = 1)
  checkmate::assert_string(patient_id, null.ok = TRUE)
  checkmate::assert_string(date_column, null.ok = TRUE)
  checkmate::assert_logical(extended, len = 1)

  stats <- list(table_name = table_name)

  #--- Total records ---
  total_records <- DBI::dbGetQuery(
    con, glue::glue("SELECT COUNT(*) FROM {table_name}")
  ) |> dplyr::pull()

  stats$total_records <- total_records

  #--- Unique patients ---
  unique_patients <- DBI::dbGetQuery(
    con, glue::glue("SELECT COUNT(DISTINCT {patient_id}) FROM {table_name}")
  ) |> dplyr::pull()

  stats$unique_patients <- unique_patients

  #--- Extended statistics ---
  if (extended) {
    records_per_patient <- DBI::dbGetQuery(con, glue::glue(
      "SELECT AVG(cnt) AS avg, MEDIAN(cnt) AS median,
              MIN(cnt) AS min, MAX(cnt) AS max
       FROM (
         SELECT {patient_id}, COUNT(*) AS cnt
         FROM {table_name}
         GROUP BY {patient_id}
       ) t1"
    ))
    stats$records_per_patient <- records_per_patient

    date_stats <- DBI::dbGetQuery(con, glue::glue(
      "SELECT MIN({date_column}) AS min_date,
              MAX({date_column}) AS max_date
       FROM {table_name}"
    ))
    stats$date_stats <- date_stats

    yearly_counts <- DBI::dbGetQuery(con, glue::glue(
      "SELECT EXTRACT(YEAR FROM {date_column}) AS year,
              COUNT(*) AS records
       FROM {table_name}
       GROUP BY year
       ORDER BY year"
    ))
    stats$yearly_counts <- yearly_counts
  }

  #--- Print summary ---
  cli::cli_alert_info("Total records: {.val {format(total_records, big.mark = ',')}}")
  cli::cli_alert_info("Unique patients: {.val {format(unique_patients, big.mark = ',')}}")

  if (extended) {
    cli::cli_alert_info("Records per patient:")
    cli::cli_li("Average: {.val {round(records_per_patient$avg, 1)}}")
    cli::cli_li("Median:  {.val {records_per_patient$median}}")
    cli::cli_li("Range:   {.val {records_per_patient$min}} to {.val {records_per_patient$max}}")

    cli::cli_alert_info("Date range:")
    cli::cli_li("From: {.val {date_stats$min_date}}")
    cli::cli_li("To:   {.val {date_stats$max_date}}")

    cli::cli_alert_info("Records by year:")
    for (i in seq_len(nrow(yearly_counts))) {
      cli::cli_li("{.val {yearly_counts$year[i]}}: {.val {format(yearly_counts$records[i], big.mark = ',')}}")
    }
  }

  invisible(stats)
}
