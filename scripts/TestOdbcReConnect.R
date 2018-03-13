## TestOdbcReConnect.R

library(tidyverse, quietly = TRUE)
library(stringr)
# library(dbplyr)
library(DBI)
library(odbc)
library(ds4ciMisc)

con <- odbc::dbConnect(odbc::odbc(), "DSA_TEP")
class(con)
gt_run_parameters_tbl <- tbl(con, dbplyr::in_schema("gt_stg", "gt_run_parameters"))
ncol(gt_run_parameters_tbl)

## now drop internet which should cause broken con

gt_run_parameters_tbl <- tbl(con, dbplyr::in_schema("gt_stg", "gt_run_parameters"))

con <- odbcReConnect(con)
class(con)
gt_run_parameters_tbl <- tbl(con, dbplyr::in_schema("gt_stg", "gt_run_parameters"))

