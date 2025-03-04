library(testthat)
library(tibble)
library(dplyr)

test_that("Warnings class initializes correctly", {
  warnings <- Warnings$new()
  expect_true(is.data.frame(warnings$data))
  expect_equal(nrow(warnings$data), 0)
  expect_equal(ncol(warnings$data), 2)
  expect_equal(names(warnings$data), c("type", "message"))
})

test_that("add_warning method adds a single warning", {
  warnings <- Warnings$new()
  warnings$add_warning("test_type", "test_message")
  expect_equal(nrow(warnings$data), 1)
  expect_equal(warnings$data$type[1], "test_type")
  expect_equal(warnings$data$message[1], "test_message")
})

test_that("add_warnings method adds multiple warnings", {
  warnings <- Warnings$new()
  new_warnings <- tibble::tibble(
    type = c("type1", "type2"),
    message = c("message1", "message2")
  )
  warnings$add_warnings(new_warnings)
  expect_equal(nrow(warnings$data), 2)
  expect_equal(warnings$data$type, c("type1", "type2"))
  expect_equal(warnings$data$message, c("message1", "message2"))
})

test_that("get_warnings method returns the correct data", {
  warnings <- Warnings$new()
  warnings$add_warning("test_type", "test_message")
  result <- warnings$get_warnings()
  expect_equal(nrow(result), 1)
  expect_equal(result$type[1], "test_type")
  expect_equal(result$message[1], "test_message")
})

test_that("add_warning method does not overwrite existing warnings", {
  warnings <- Warnings$new()
  warnings$add_warning("test_type1", "test_message1")
  warnings$add_warning("test_type2", "test_message2")
  expect_equal(nrow(warnings$data), 2)
  expect_equal(warnings$data$type, c("test_type1", "test_type2"))
  expect_equal(warnings$data$message, c("test_message1", "test_message2"))
})

test_that("add_warnings method does not overwrite existing warnings", {
  warnings <- Warnings$new()
  warnings$add_warning("test_type1", "test_message1")
  new_warnings <- tibble::tibble(
    type = c("type2", "type3"),
    message = c("message2", "message3")
  )
  warnings$add_warnings(new_warnings)
  expect_equal(nrow(warnings$data), 3)
  expect_equal(warnings$data$type, c("test_type1", "type2", "type3"))
  expect_equal(warnings$data$message, c("test_message1", "message2", "message3"))
})

test_that("add_warnings method strips excess columns", {
  warnings <- Warnings$new()
  new_warnings <- tibble::tibble(
    type = c("type1", "type2"),
    message = c("message1", "message2"),
    extra_column = c("extra1", "extra2")
  )
  warnings$add_warnings(new_warnings)
  expect_equal(nrow(warnings$data), 2)
  expect_equal(names(warnings$data), c("type", "message"))
  expect_equal(warnings$data$type, c("type1", "type2"))
  expect_equal(warnings$data$message, c("message1", "message2"))
})
