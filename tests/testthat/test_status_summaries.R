library(testthat)

test_that("station and city changes are read from the change column", {
  station_statuses <- tibble::tibble(
    id = paste0("s", 1:4),
    name = c("Old", "Brand new", "Back again", "Gone"),
    city_id = c("delhi", "leh", "agra", "pune"),
    city_name = c("Delhi", "Leh", "Agra", "Pune"),
    status = c("live", "live", "live", "inactive"),
    change = c("No change", "New", "Reactivated", "Removed this month"),
    percent_complete = c(1, 1, 1, 0),
    percent_category = c(">80% data", ">80% data", ">80% data", "No data")
  )
  warnings <- Warnings$new()

  summary <- summarise_station_and_city_statuses(
    station_statuses = station_statuses,
    location_presets = tibble::tibble(name = "ncap_cities", location_id = "delhi"),
    warnings = warnings
  )

  expect_match(summary, "- New stations: Brand new\n", fixed = TRUE)
  expect_match(summary, "- Reactivated stations: Back again\n", fixed = TRUE)
  expect_match(summary, "- Removed stations: Gone\n", fixed = TRUE)
  expect_match(summary, "- Cities with new stations: Leh\n", fixed = TRUE)
  expect_true("new_cities" %in% warnings$get_warnings()$type)
})
