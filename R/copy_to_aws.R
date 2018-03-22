#' Copy a Local Data Frame to a DBI Backend on AWS.
#'
#' This \code{copy_to_aws()} method assumes an AWS Redshift database with is accessable from S3. Unlike
#' \code{dbplyr::copy_to()}, on which it is loosly modeled, it is optimized for large dataframes. It is
#' essentially a bulk loader of R dataframes, or tbls, to a Redshift table - which may, or may not, already
#' exist. \strong{For initial release table must exist!}
#'
#' \code{copy_to_aws()} leverages the Redshift \code{COPY} command of a pipe-delimited gzip'd file of the
#' dataframe which it uploads to a specified S3 bucket and folder. The file is deleted if no error is thrown.
#'
#' \strong{Assumptions:}
#'
#' \itemize{
#'   \item INITIAL RELEASE ASSUMES TARGET schema.table EXISTS
#'   \item The S3 credentials have been set up as described in \url{https://github.com/cloudyr/aws.s3}
#' }
#'
#'
#' @param con A DBI, or odbc, connection to the AWS Redshift database to copy to
#' @param df The data.frame or tbl_df to copy
#' @param schema The schema in the datbase
#' @param tname The table name in the schema
#' @param s3_bucket The S3 bucket to use as a "buffer"
#' @param s3_folder A folder in the bucket. Will be created if it doesn't exist already
#' @param overwrite TRUE to truncate the table before \code{COPY}ing
#' @param clear_s3 TRUE deletes the temp s3 file before returning
#' @param temporary Placeholder - not used in initial release
#' @param identity_column Placeholder - not used in initial release
#' @param types Placeholder - not used in initial release
#'
#' @return if the \code{COPY} was successful, the number of rows copied. If unsuccessful, the Redshift error.
#'
#' @examples
#'
#' @export
copy_to_aws <- function(con, df, schema = "public", tname = deparse(substitute(df)),
                        s3_bucket, s3_folder = "ds4ci_temp",
                        overwrite = FALSE, clear_s3 = TRUE, temporary = FALSE,
                        identity_column = FALSE,
                        types = NULL,
                        ...) {
  if(!("data.frame" %in% class(df))) stop("'df' not a data.frame")

  tfile <- paste0(tempfile(), ".txt.gz")
  s3_obj <- paste0(s3_folder, "/", stringr::word(tfile, -1, sep = fixed("\\")))
  vals <- list(schema = schema,
               tname = tname,
               s3b = s3_bucket,
               s3o = s3_obj,
               ak_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
               sak = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
  q_nrows <- "
  SELECT COUNT(*)
  FROM ${schema}$.${tname}$;
  "
  qnr <- ds4ciMisc::sql_varsub(q_nrows, vals)

  if(overwrite){
    q_trunc <- "
    TRUNCATE TABLE ${schema}$.${tname}$;
    "
    qt <- ds4ciMisc::sql_varsub(q_trunc, vals)
    ret_e <- DBI::dbExecute(con, qt)
  }

  nrow_initial <- DBI::dbGetQuery(con, qnr)[1,1]

  readr::write_delim(df, gzfile(tfile), delim = "|", na = "")
  ret_put <- aws.s3::put_object(tfile, s3_obj, s3_bucket)

  q_copy <- "
  COPY ${schema}$.${tname}$
  FROM 's3://${s3b}$/${s3o}$'
  ACCESS_KEY_ID '${ak_id}$' SECRET_ACCESS_KEY '${sak}$'
  TIMEFORMAT 'auto'
  GZIP
  IGNOREHEADER 1;
  "
  q <- ds4ciMisc::sql_varsub(q_copy, vals)
  ret_c <- DBI::dbExecute(con, q)

  nrow_final <- DBI::dbGetQuery(con, qnr)[1,1]
  nrow_inserted <- nrow_final - nrow_initial
  if(nrow_inserted != nrow(df)) stop(paste0(nrows, " rows inserted. Expected ", nrow(df)))

  # delete local temp file
  file.remove(tfile)

  # optionally drop S3 temp file
  if(clear_s3){
    ret_do <- aws.s3::delete_object(s3_obj, s3_bucket)
    if(is.na(as.logical(ret_do))) stop()
  }

  as.integer(nrow_inserted)
}