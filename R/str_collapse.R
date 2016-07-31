#' Collapse a Multi-line SQL Query
#'
#' Collapses a well formatted SQL query such as you would get with
#' \code{readLines()} reading a query created with, say, dbVis into
#' a single character string. Also any "to end of line" comments are
#' removed before doing the collapse.
#'
#' @param query string The multi-line SQL query with, perhaps, "--" comments.
#'
#' @examples
#' \dontrun{
#' q <- readLines('./SQL/qVariablesInQueryExample1.sql')
#' sql_collapse(q)
#' }
#' @export
sql_collapse <- function(query){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  q <- str_replace(query, "--[:print:]*$", "")
  str_trim(paste(q, collapse = " "))
}