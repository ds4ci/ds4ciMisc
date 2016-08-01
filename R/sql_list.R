#' Convert R vector to Single SQL Query Clause String.
#'
#' \code{sql_list()} Returns a character string which can be pasted into
#' a SQL query without throwing an error.
#'
#' Usually used in SQL WHERE clause as
#' \code{paste("... WHERE <exp> IN", sql_list(vector))}.

#' @param MyVector A single dimension vector. Will be cast to character with
#'   \code{as.character()}
#' @return A single character sting of elements in the input vector surrounded
#'   by parens. Each element is enclosed in single quotes and seperated by
#'   commas.
#' @examples
#' sql_list(c(1, 2, 3))
#' sql_list(c("a", "b", "c"))
#'
#' @export
sql_list <- function(MyVector){
  sl <- as.character(MyVector)
  sl <- str_replace(str_replace(sl, "^", "'"), "$", "'")
  paste0("(", paste(sl, collapse=", "), ")" )
}