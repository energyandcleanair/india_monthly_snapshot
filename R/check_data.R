
check_data <- function(
    ...,
    warnings,
    city_measurements,
    station_measurements,
    location_presets,
    day_threshold,
    focus_month) {
  log_debug("Checking number of cities doesn't exceed the limit")
  check_data_city_limits(
    city_measurements = city_measurements,
    location_presets = location_presets,
    day_threshold = day_threshold
  )

  log_debug("Checking winter PM2.5 upper limits")
  check_data_winter_pm25(
    warnings = warnings,
    city_measurements = city_measurements,
    focus_month = focus_month
  )

  log_debug("Checking if southern cities are in the top 10")
  check_data_top_10_cities(
    warnings = warnings,
    city_measurements = city_measurements
  )

  log_debug("Checking for low PM2.5 values")
  check_data_values_low(
    warnings = warnings,
    city_measurements = city_measurements
  )

  log_debug("Checking for duplicate station daily entries")
  check_data_duplicate_entries(
    station_measurements = station_measurements
  )
}

check_data_city_limits <- function(..., city_measurements, location_presets, day_threshold) {
  cities <- city_measurements %>%
    group_by(city_id) %>%
    summarise()
  if (nrow(cities) > max_cities_india) {
    stop(glue(
      "Number of cities ({nrow(cities)}) from the API exceed the limit ({max_cities_india})"
    ))
  }

  ncap_cities <- location_presets %>%
    filter(name == "ncap_cities") %>%
    inner_join(city_measurements, by = "location_id") %>%
    distinct(location_id)
  if (nrow(ncap_cities) > max_cities_ncap) {
    stop(glue(
      "Number of NCAP cities ({nrow(ncap_cities)}) from the API exceed the limit {max_cities_ncap}"
    ))
  }

  days_in_analysis <- city_measurements %>%
    group_by(date) %>%
    summarise()
  if (nrow(days_in_analysis) < day_threshold) {
    stop(glue(
      "Number of days ({nrow(days_in_analysis)}) in the month is less than 80% ({day_threshold})"
    ))
  }
}

check_data_winter_pm25 <- function(..., city_measurements, warnings, focus_month) {
  winter_months <- c(10, 11, 12, 1, 2)
  month_of_year <- lubridate::month(focus_month)

  if (month_of_year %in% winter_months) {
    average_pm25 <- city_measurements %>%
      group_by(city_id) %>%
      summarise(
        average_pm25 = mean(value, na.rm = TRUE)
      )

    if (any(average_pm25$average_pm25 > 100)) {
      high_pm25_cities <- average_pm25 %>%
        filter(average_pm25 > 100)

      warnings$add_warnings(
        high_pm25_cities %>%
          mutate(
            type = "high_pm25_gt100",
            message = glue(
              "City {city_id} has an average PM2.5 concentration above 100 µg/m³: {average_pm25}"
            )
          )
      )
    }

    if (any(average_pm25$average_pm25 > 200)) {
      very_high_pm25_cities <- average_pm25 %>%
        filter(average_pm25 > 200)

      warnings$add_warnings(
        very_high_pm25_cities %>%
          mutate(
            type = "high_pm25_gt200",
            message = glue(
              "City {city_id} has an average PM2.5 concentration above 200 µg/m³: {average_pm25}"
            )
          )
      )
    }
  }
}

check_data_top_10_cities <- function(..., city_measurements, warnings) {
  top_10_cities <- city_measurements %>%
    group_by(city_id, city_name, gadm1_id) %>%
    summarise(
      average_pm25 = mean(value, na.rm = TRUE)
    ) %>%
    arrange(desc(average_pm25)) %>%
    head(10)

  if (any(top_10_cities$gadm1_id %in% names(south_india_states))) {
    warnings$add_warnings(
      top_10_cities %>%
        filter(gadm1_id %in% names(south_india_states)) %>%
        mutate(
          type = "southern_city_in_top_10",
          message = glue("City {city_name} is in the top 10")
        )
    )
  }
}

check_data_values_low <- function(..., city_measurements, warnings) {
  low_value_measurements <- city_measurements %>%
    filter(value < low_value_threshold)

  if (nrow(low_value_measurements) > 0) {
    warnings$add_warnings(
      low_value_measurements %>%
        mutate(
          type = "low_value_lt5",
          message = glue("City {city_id} on {date} has a value less than 5: {value}")
        )
    )
  }

  moderate_value_measurements <- city_measurements %>%
    filter(value >= low_value_threshold & value < moderate_value_threshold)

  if (nrow(moderate_value_measurements) > 0) {
    warnings <- warnings$add_warnings(
      moderate_value_measurements %>%
        mutate(
          type = "low_value_lt10",
          message = glue("City {city_id} on {date} has a value between 5 and 10: {value}")
        )
    )
  }

  return(warnings)
}

check_data_duplicate_entries <- function(..., station_measurements) {
  duplicate_entries <- station_measurements %>%
    group_by(location_id, date) %>%
    filter(n() > 1)

  if (nrow(duplicate_entries) > 0) {
    formatted_entries <- paste(
      duplicate_entries$location_id,
      duplicate_entries$date,
      sep = " on ",
      collapse = ", "
    )

    stop(glue(
      "Duplicate entries found for the following locations and dates: {formatted_entries}"
    ))
  }
}
