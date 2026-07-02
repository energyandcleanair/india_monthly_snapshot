library(testthat)
library(mockery)

test_that("cache_data_csv: caches data correctly when cache file does not exist", {
  mock_read_csv <- mock(data.frame(a = 1:3), cycle = TRUE)
  mock_write_csv <- mock()
  stub(cache_data_csv, "readr::read_csv", mock_read_csv)
  stub(cache_data_csv, "write.csv", mock_write_csv)
  stub(cache_data_csv, "file.exists", FALSE)

  url <- "http://example.com/data.csv"
  cache_file <- "cache/data.csv"
  data <- cache_data_csv(url, cache_file)

  expect_equal(data, data.frame(a = 1:3))
  expect_called(mock_read_csv, 2)
  expect_called(mock_write_csv, 1)
})

test_that("cache_data_csv: caches data correctly when cache file exists", {
  mock_read_csv <- mock(data.frame(a = 1:3), cycle = TRUE)
  mock_write_csv <- mock()
  stub(cache_data_csv, "readr::read_csv", mock_read_csv)
  stub(cache_data_csv, "write.csv", mock_write_csv)
  stub(cache_data_csv, "file.exists", TRUE)

  url <- "http://example.com/data.csv"
  cache_file <- "cache/data.csv"
  data <- cache_data_csv(url, cache_file)

  expect_equal(data, data.frame(a = 1:3))
  expect_called(mock_read_csv, 1)
  expect_called(mock_write_csv, 0)
})

test_that("fetch_data_csv: fetches data correctly with use_cache = TRUE", {
  mock_cache_data <- mock(data.frame(a = 1:3))
  stub(fetch_data_csv, "cache_data_csv", mock_cache_data)

  url <- "http://example.com/data.csv"
  cache_file <- "cache/data.csv"
  data <- fetch_data_csv(url, cache_file, use_cache = TRUE)

  expect_equal(attr(data, "url"), url)
  attr(data, "url") <- NULL
  expect_equal(data, data.frame(a = 1:3))
  expect_called(mock_cache_data, 1)
})

test_that("fetch_data_csv: fetches data correctly with use_cache = FALSE", {
  mock_read_csv <- mock(data.frame(b = 4:6))
  stub(fetch_data_csv, "readr::read_csv", mock_read_csv)

  url <- "http://example.com/data.csv"
  cache_file <- "cache/data.csv"
  data <- fetch_data_csv(url, cache_file, use_cache = FALSE)

  expect_equal(attr(data, "url"), url)
  attr(data, "url") <- NULL
  expect_equal(data, data.frame(b = 4:6))
  expect_called(mock_read_csv, 1)
})

test_that("validate: function returns error with missing columns", {
  data <- data.frame(
    date = as.Date("2021-01-01"),
    variable = "observed",
    value = 10
  )

  expect_error(validate(data, columns = c("date", "missing_column")))
})

test_that("validate: returns input data even when rules fail", {
  data <- data.frame(
    date = as.Date("2021-01-01"),
    variable = "observed",
    value = 10
  )

  validated_data <- validate(
    data,
    rules = validate::validator(
      all(variable == "predicted")
    )
  )

  expect_equal(validated_data, data)
})

test_that("validate: function validates data correctly with valid data", {
  data <- data.frame(
    date = as.Date("2021-01-01"),
    variable = "observed",
    value = 10
  )

  measurement_columns <- c("date", "variable", "value")

  measurement_rules <- validate::validator(
    !is.na(as.Date(date, format = "%Y-%m-%d")),
    variable %in% c("observed", "predicted"),
    is.numeric(value) && value >= 0
  )
  validated_data <- validate(data, columns = measurement_columns, rules = measurement_rules)
  expect_equal(validated_data, data)
})
