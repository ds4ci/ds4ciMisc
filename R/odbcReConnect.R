#' If ODBC connection not functioning, re-connect.
#'
#' Attempts to create a dplyr::tbl() object. If that fails, then odbc::dbConnect() to
#' re-connect.
#'
#' Note: odbc::IsValid does not check the connection is actually still live, which
#' is why we use dplyr::tbl() to test if live.
#'
#' Will throw error \emph(Could not re-establish connection.), if new connection is not
#' \code(identical()) to original.
#'
#' @param con An established DBI/odbc connection object
#' @param on_table The DB table, possibly in a schema, to use for test.
#'
#' @return TRUE when connection is up, perhaps after re-connecting.
#' @export
#'
#' @examples
odbcReConnect <- function(con,
                        on_table = in_schema("gt_stg", "gt_run_parameters")){
  rt <- try(tbl(con, on_table), silent = TRUE)
  if(rt == "try-error"){
    dsa <- con@info$sourcename
    rc <- odbc::dbConnect(odbc::odbc(), dsa)
    if(!identical(rc, con)) stop("Could not re-establish connection.")
  }
  TRUE
}