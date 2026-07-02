library(testthat)
library(dplyr)

test_that("summarise_top_polluted_city_frequency disambiguates duplicate city names", {
  measurements <- tibble::tibble(
    date = as.Date(c(
      "2026-06-01", "2026-06-01", "2026-06-01",
      "2026-06-02", "2026-06-02", "2026-06-02"
    )),
    pollutant = "pm25",
    pollutant_name = "PM2.5",
    value = c(90, 80, 70, 95, 85, 75),
    location_id = c(
      "bilaspur_ind.7_1_in", "bilaspur_ind.14_1_in", "raipur_ind.7_1_in",
      "bilaspur_ind.7_1_in", "bilaspur_ind.14_1_in", "raipur_ind.7_1_in"
    ),
    city_name = c("Bilaspur", "Bilaspur", "Raipur", "Bilaspur", "Bilaspur", "Raipur"),
    gadm1_name = c(
      "Chhattisgarh", "Himachal Pradesh", "Chhattisgarh",
      "Chhattisgarh", "Himachal Pradesh", "Chhattisgarh"
    )
  )

  result <- summarise_top_polluted_city_frequency(measurements)

  expect_equal(
    result$location_id,
    c("bilaspur_ind.7_1_in", "bilaspur_ind.14_1_in", "raipur_ind.7_1_in")
  )
  expect_equal(
    result$city_label,
    c(
      "Bilaspur, CG",
      "Bilaspur, HP",
      "Raipur"
    )
  )
  expect_equal(result$count, c(2L, 2L, 2L))
  expect_equal(anyDuplicated(result$city_label), 0L)
})

test_that("summarise_top_polluted_city_frequency keeps unique city labels compact", {
  measurements <- tibble::tibble(
    date = as.Date(c("2026-06-01", "2026-06-01")),
    pollutant = "pm25",
    pollutant_name = "PM2.5",
    value = c(90, 80),
    location_id = c("delhi_ind.25_1_in", "mumbai_ind.20_1_in"),
    city_name = c("Delhi", "Mumbai"),
    gadm1_name = c("Delhi", "Maharashtra")
  )

  result <- summarise_top_polluted_city_frequency(measurements)

  expect_equal(result$city_label, c("Delhi", "Mumbai"))
  expect_equal(result$count, c(1L, 1L))
})

test_that("summarise_top_polluted_city_frequency counts cities by location ID", {
  measurements <- tibble::tibble(
    date = as.Date(c("2026-06-01", "2026-06-01")),
    pollutant = "pm25",
    pollutant_name = "PM2.5",
    value = c(90, 80),
    location_id = c("city_a", "city_b"),
    city_name = c("Shared Name", "Shared Name"),
    gadm1_name = c("Delhi", "Delhi")
  )

  result <- summarise_top_polluted_city_frequency(measurements)

  expect_equal(result$location_id, c("city_a", "city_b"))
  expect_equal(result$count, c(1L, 1L))
})
