#' Substitute Values into a SQL Query.
#'
#' Substitutes given values into a SQL query string containing embeded
#' dbVis style variables. The simple variable formats are \code{${name}$}
#' and, when a the variable has a default, \code{${name||default}$}.
#'
#' \href{http://www.dbvis.com/}{dbVisualizer} is Jim's prefered SQL front end
#' tool for SQL DBMS work. One nice feature is it supports named variables in
#' query which are filled in at run time via a dialog box. This makes it very
#' easy to develop paramertized general purpose SQL queries which can be
#' cut & pasted into R or, preforably, stored in your project's SQL folder ready
#' for \code{readLines()} into a string and then paramatized with
#' \code{sql_varsub}.
#'
#' If the query string has a variable which is not named in the values list,
#' it is filled with the default for the variable. If a variable is used multiple
#' times in the query, only one instance will need to have the default. When there
#' is no defaults for a variable not named in the values list, an an error is thrown.
#'
#' See dbVis help page by Googling #' "dbvisualizer variables x.x", where x.x is the
#' verion of dbVis.
#'
#' Note that the list \code{values} may have unused elements. Thus, best practice is
#' to have a common list for a set of similar queries (eg SELECT, COPY, TRUNCATE, etc)
#'
#' @param query string The query with embeded dbVis variables.
#' @param values A list of named value(s) where the name(s) are
#'     variable names in the query.
#' @param collapse = TRUE Run \code{sql_collapse()} as first step.
#' @examples
#' \dontrun{
#' # simple query
#' sql_varsub("SELECT COUNT(*) FROM ${table}", list(table = "my_table"))
#' # more realistic
#' (q <- readLines('./SQL/qVariablesInQueryExample1.sql'))
#' [1] "-- qVariablesInQueryExample1.sql"
#' [2] ""
#' [3] "SELECT COUNT(DISTINCT ${column||product_id}$) AS number_unique_${column}$"
#' [4] "  FROM ${table}$"
#' [5] ";"
#' v <- list(table = "sleeping_dogs.sd_text", column = "date_is")
#' (q <- sql_varsub(q, v))
#' [1] "SELECT COUNT(DISTINCT date_is) AS number_unique_date_is   FROM sleeping_dogs.sd_text ;"
#' dbGetQuery(conn, q)    ## assuming connection is set up already
#' }
#'
#' @export
sql_varsub <- function(query, values, collapse = TRUE){
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (is.na(query)) stop("argument 'query' is NA")
  if(anyNA(values)) stop("argument 'values' has at least one NA")

  q <- query
  if (collapse) q <- sql_collapse(q)
  v <- values
  for(variable in names(v)) {
    # q1 <- stringr::str_replace(q, paste0("${", variable, "[|A-Za-z_]*}$"), v[[variable]])
    # q <- gsub(paste0("\\$\\{", variable, "[|A-Za-z_]*\\}\\$"), v[[variable]], q, ignore.case = TRUE)
    q <- stringr::str_replace_all(q,
                                  paste0("\\$\\{", variable, "[|[:alnum:]_]*\\}\\$"),
                                  v[[variable]])

  }
  # If no more dbVis variables, return now.
  if(!stringr::str_detect(q, "\\$\\{")) return(q)

  # Use defaults in unresolved dbVis variables
  # q <- "SELECT COUNT(DISTINCT ${column||product_id}$) AS number_unique_${column}$   FROM sleeping_dogs.sd_text WHERE ${where_date}$ <= '2016-01-01';" ## debug
  qs <- q %>%
    stringr::str_extract_all("\\$\\{[|[:alnum:]_]*\\}\\$") %>%
    unlist() %>%
    stringr::str_replace_all("[\\$\\{\\}]", "") %>%
    stringr::str_split_fixed(fixed("||"), n = 2) %>%
    tibble::as_tibble()
  colnames(qs) <- c("variable", "default")

  qs_with_default <- qs %>%
    dplyr::filter(default != "") %>%
    dplyr::distinct()
  for(variable in qs_with_default["variable"]) {
    default <- dplyr::filter(qs_with_default, variable == variable)[["default"]]
    q <- stringr::str_replace_all(q,
                                  paste0("\\$\\{", variable, "[|[:alnum:]_]*\\}\\$"),
                                  default)
  }
  # If no more dbVis variables, return now.
  if(!stringr::str_detect(q, "\\$\\{")) return(q)

  stop(paste("Unresolved dbVis variables:", q))
}