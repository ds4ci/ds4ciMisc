#' If ODBC connection not functioning, re-connect.
#'
#' Attempts to create a dplyr::tbl() object. If that fails, then odbc::dbConnect() to
#' re-connect.
#'
#' Note: \code{odbc::IsValid} does not check the connection is actually still live, which
#' is why we use \code{dplyr::tbl()} to test.
#'
#' Will throw error \emph{Could not re-establish connection.}, if new connection is not
#' \code{identical()} to original.
#'
#' @param con An established DBI/odbc connection object
#' @param on_table The DB table, possibly in a schema, to use for test.
#'
#' @return The original, or re-estbalished, connection.
#'
#' @export
odbcReConnect <- function(con,
                          on_table = dbplyr::in_schema("pg_catalog", "pg_type")){
  rt <- try(tbl(con, on_table), silent = TRUE)
  if(class(rt)[1] == "try-error"){
    dsa <- con@info$sourcename
    odbc::dbDisconnect(con)
    rc <- odbc::dbConnect(odbc::odbc(), dsa)
    if(!identical(rc@info, con@info)) stop("Could not re-establish connection.")
    return(rc)
  } else {
    return(con)
  }
}