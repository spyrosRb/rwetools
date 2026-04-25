#' Render an Interactive DataTable
#'
#' Renders a styled interactive table using the `DT` package, with a branded
#' header background colour and configurable pagination and layout.
#'
#' @param data A data frame to render.
#' @param colnames An optional character vector of display column names.
#'   Defaults to the column names of `data`.
#' @param options An optional list of DT options. When empty, a default set is
#'   applied using `pageLength` and `dom`.
#' @param filter A string specifying filter placement: `"none"` (default),
#'   `"top"`, or `"bottom"`.
#' @param pageLength An integer specifying the number of rows per page.
#'   Default `100`.
#' @param dom A string specifying the table control layout. Default `"t"`
#'   (table only).
#'
#' @return A `DT::datatable` object.
#' @export
#'
#' @examples
#' \dontrun{
#' render_data_table(mtcars)
#' render_data_table(mtcars, pageLength = 10, dom = "tip")
#' }
render_data_table <- function(data,
                              colnames   = NULL,
                              options    = list(),
                              filter     = "none",
                              pageLength = 100,
                              dom        = "t") {

  #--- Argument checks ---
  checkmate::assert_data_frame(data)
  checkmate::assert_character(colnames, null.ok = TRUE)
  checkmate::assert_list(options)
  checkmate::assert_choice(filter, c("none", "top", "bottom"))
  checkmate::assert_count(pageLength, positive = TRUE)
  checkmate::assert_string(dom)

  #--- Set column display names ---
  if (is.null(colnames)) colnames <- names(data)

  #--- Build DT options ---
  if (length(options) == 0) {
    options <- list(
      pageLength  = pageLength,
      dom         = dom,
      bFilter     = 0,
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#1482FA', 'color': 'white'});",
        "}"
      )
    )
  } else {
    options$pageLength <- pageLength
    options$dom        <- dom
  }

  #--- Render and return DataTable ---
  DT::datatable(
    data,
    colnames = colnames,
    options  = options,
    filter   = filter
  )
}
