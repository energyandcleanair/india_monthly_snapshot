library(testthat)

test_that("old city IDs and names become the current ones", {
  presets <- tibble::tibble(name = "ncap_cities", location_id = c("bangalore_ind.16_1_in", "x"))
  expect_equal(normalise_city_ids(presets)$location_id, c("bengaluru_ind.16_1_in", "x"))

  stations <- tibble::tibble(
    city_id = c("bangalore_ind.16_1_in", NA),
    city_name = c("Bangalore", NA)
  )
  expect_equal(normalise_city_ids(stations)$city_id, c("bengaluru_ind.16_1_in", NA))
  expect_equal(normalise_city_ids(stations)$city_name, c("Bengaluru", NA))
})

test_that("a day under both IDs keeps the current ID's value", {
  measurements <- tibble::tibble(
    date = as.Date(c("2023-08-01", "2023-08-01", "2023-08-02", "2023-08-01")),
    location_id = c("bangalore_ind.16_1_in", "bengaluru_ind.16_1_in", "bangalore_ind.16_1_in", "x"),
    city_id = location_id,
    city_name = c("Bangalore", "Bengaluru", "Bangalore", "X"),
    value = c(10, 11, 12, 5)
  )
  result <- normalise_city_measurements(measurements) %>% dplyr::arrange(location_id, date)
  expect_equal(result$location_id, c(rep("bengaluru_ind.16_1_in", 2), "x"))
  expect_equal(result$value, c(11, 12, 5))
  expect_equal(unique(result$city_name[1:2]), "Bengaluru")
})
