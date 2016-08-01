library(ds4ciMisc)
context("SQL variable substitution")

test_that("Simple variables are substituted correctly",{
  in_string <- "SELECT * FROM ${table1}$ WHERE ${column1}$ IS NOT NULL;"
  value_list <- list(column1 = "sold_on", table1 = "sales")
  out_string <- "SELECT * FROM sales WHERE sold_on IS NOT NULL;"
  expect_match(sql_varsub(in_string, value_list), out_string, fixed = TRUE)
})j

test_that("Defaults not used when values given", {
  in_string <- "SELECT * FROM ${table1||xxxx}$ WHERE ${column1||yyyy}$ IS NOT NULL;"
  value_list <- list(column1 = "sold_on", table1 = "sales")
  out_string <- "SELECT * FROM sales WHERE sold_on IS NOT NULL;"
  expect_match(sql_varsub(in_string, value_list), out_string, fixed = TRUE)
})

test_that("Default picked up for other instances", {
  in_string <- "SELECT ${column1}$ FROM ${table1||xxxx}$ WHERE ${column1||sold_on}$ IS NOT NULL;"
  value_list <- list(table1 = "sales")
  out_string <- "SELECT sold_on FROM sales WHERE sold_on IS NOT NULL;"
  expect_match(sql_varsub(in_string, value_list), out_string, fixed = TRUE)
})

test_that("Nothing to do is handled OK",{
  in_string <- "SELECT * FROM sales WHERE sold_on IS NOT NULL;"
  value_list <- list(dummy = "xxxxx")
  out_string <- "SELECT * FROM sales WHERE sold_on IS NOT NULL;"
  expect_match(sql_varsub(in_string, value_list), out_string, fixed = TRUE)
})