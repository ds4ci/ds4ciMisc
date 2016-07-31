library(ds4ciMisc)
context("SQL variable substitution")

test_that("Simple variables are substituted correctly",{
  in_string <- "SELECT * FROM ${table1}$ WHERE ${column1}$ IS NOT NULL;"
  value_list <- list(column1 = "sold_on", table1 = "sales")
  out_string <- "SELECT * FROM sales WHERE sold_on IS NOT NULL;"
  expect_match(sql_varsub(in_string, value_list), out_string, fixed = TRUE)
})
