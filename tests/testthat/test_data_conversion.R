library(testthat)
library(dplyr)

test_that("unnest_json_columns parses Python literal dictionaries", {
  data <- tibble::tibble(
    infos = "{'latest_data': '2026-07-02 10:00:00', 'status': 'Live'}"
  )

  result <- unnest_json_columns(data, "infos")

  expect_equal(result$latest_data, "2026-07-02 10:00:00")
  expect_equal(result$status, "Live")
})

test_that("unnest_json_columns preserves apostrophes in string values", {
  data <- tibble::tibble(
    infos = paste0(
      "{'address': \"Talkatora Garden, \\nPresident's Estate, New Delhi\", ",
      "'latest_data': '2026-07-02 10:00:00', ",
      "'status': 'Live'}"
    )
  )

  result <- unnest_json_columns(data, "infos")

  expect_equal(
    result$address,
    "Talkatora Garden, \nPresident's Estate, New Delhi"
  )
  expect_equal(result$status, "Live")
})

test_that("unnest_json_columns supports JSON and Python null values", {
  data <- tibble::tibble(
    infos = c(
      "{\"latest_data\": \"2026-07-02 10:00:00\", \"status\": \"Live\"}",
      "{'latest_data': None, 'status': 'Inactive'}",
      NA_character_
    )
  )

  result <- unnest_json_columns(data, "infos")

  expect_equal(result$status, c("Live", "Inactive", NA))
  expect_equal(result$latest_data[[1]], "2026-07-02 10:00:00")
  expect_true(is.na(result$latest_data[[2]]))
  expect_true(is.na(result$latest_data[[3]]))
})
