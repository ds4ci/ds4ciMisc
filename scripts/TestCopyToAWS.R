## TestCopyToAWS.R

library(tidyverse, quietly = TRUE)
library(stringr)
library(dbplyr)
library(DBI)
library(odbc)
library(ds4ciMisc)
library(lfGT)

## helpers

nrows_in_temp <- function(){
  nrows_init <- gt_run_params_temp_tbl %>%
    summarise(n = as.integer(n())) %>%
    collect() %>%
    pull()
}

## connect
con <- odbc::dbConnect(odbc::odbc(), "DSA_TEP")
class(con)
gt_run_parameters_tbl <- tbl(con, in_schema("gt_stg", "gt_run_parameters"))
s3_bucket <- "lfl-dsa-bucket"
lfGT::init_s3_lf()


## in old(pre 2018-03-20) DAS_TEP instance


## initialize temp table with a few rows
q_dt <- "DROP TABLE IF EXISTS gt_temp.gt_run_params_temp;"
ret_dt <- DBI::dbExecute(con, q_dt)
ret_dt

q_ct <- "
CREATE TABLE gt_temp.gt_run_params_temp AS
SELECT *
  FROM gt_stg.gt_run_parameters
 WHERE run_at < '2017-10-01'
 ORDER BY run_at
; "
ret_ct <- DBI::dbExecute(con, q_ct)
ret_ct

gt_run_params_temp_tbl <- tbl(con, in_schema("gt_temp", "gt_run_params_temp"))

(nrows_in_temp())

## Get a data frame from orig table

load_me <- gt_run_parameters_tbl %>%
  top_n(25, run_at) %>%
  collect()

## append it

ret_a <- ds4ciMisc::copy_to_aws(con, load_me, "gt_temp", "gt_run_params_temp",
                                s3_bucket, s3_folder = "gt_temp",
                                overwrite = FALSE, clear_s3 = TRUE)

ret_a
(nrows_in_temp())

## overwrite it
ret_o <- ds4ciMisc::copy_to_aws(con, load_me, "gt_temp", "gt_run_params_temp",
                                s3_bucket, s3_folder = "gt_temp",
                                overwrite = TRUE, clear_s3 = TRUE)

ret_o
(nrows_in_temp())



