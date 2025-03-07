summarise_station_and_city_statuses <- function(
  ...,
  station_statuses,
  location_presets
) {

  key_stats <- ""

  add_header <- function(header) {
    if (key_stats != "") {
      key_stats <<- paste0(key_stats, "\n")
    }
    key_stats <<- paste0(key_stats, "## ", header, "\n\n")
  }
  add_stat <- function(stat, value) {
    key_stats <<- paste0(key_stats, "- ", stat, ": ", value, "\n")
  }

  # We need these stats:
  # - Total # of CAAQMS installed in India (live or delay)
  # - # of operation CAAQMS with >80% data
  # - # of operation CAAQMS with <80% data but >1% data
  # - # of operation CAAQMS with <1% data

  add_header("Station Statuses")
  local({
    total_stations <- nrow(station_statuses)
    add_stat("CAAQMS installed in India", total_stations)

    stations_with_gt80_data <- station_statuses %>%
      filter(percent_category == ">80% data")
    add_stat("CAAQMS with >80% data", nrow(stations_with_gt80_data))

    stations_with_lt80_data <- station_statuses %>%
      filter(percent_category == "<80% data")
    add_stat("CAAQMS with <80% data but >1% data", nrow(stations_with_lt80_data))

    stations_with_no_data <- station_statuses %>%
      filter(percent_category == "No data")
    add_stat("CAAQMS with <1% data", nrow(stations_with_no_data))
  })

  add_header("City Statuses")
  local({

    cities <- station_statuses %>%
      group_by(city_id) %>%
      summarise(
        percent_complete = max(percent_complete)
      ) %>%
      mutate(
        percent_category = percent_categoriser(percent_complete)
      )

    total_cities <- nrow(cities)
    add_stat("Cities covered by CAAQMS", total_cities)

    cities_with_gt80_data <- cities %>%
      filter(percent_category == ">80% data")
    add_stat("Cities with >80% data", nrow(cities_with_gt80_data))

    cities_with_lt80_data <- cities %>%
      filter(percent_category == "<80% data" & percent_complete > 0.01)
    add_stat("Cities with <80% data but >1% data", nrow(cities_with_lt80_data))

    cities_with_no_data <- cities %>%
      filter(percent_category == "No data")
    add_stat("Cities with <1% data", nrow(cities_with_no_data))

  })

  add_header("Changes in Stations")
  local({
    new_stations <- station_statuses %>%
      filter(status == "New")
    add_stat("New stations", paste(new_stations$name, collapse = ", "))

    reactivated_stations <- station_statuses %>%
      filter(status == "Reactivated")
    add_stat("Reactivated stations", paste(reactivated_stations$name, collapse = ", "))

    removed_stations <- station_statuses %>%
      filter(status == "Removed this month")
    add_stat("Removed stations", paste(removed_stations$name, collapse = ", "))
  })

  add_header("NCAP cities")
  local({
    ncap_cities <- location_presets %>%
      filter(name == "ncap_cities")

    ncap_city_stats <- station_statuses %>%
      filter(city_id %in% ncap_cities$location_id)
    
    ncap_city_stats <- ncap_city_stats %>%
      group_by(city_id) %>%
      summarise(
        percent_complete = max(percent_complete)
      ) %>%
      mutate(
        percent_category = percent_categoriser(percent_complete)
      )
    
    total_ncap_cities <- nrow(ncap_city_stats)
    add_stat("NCAP cities covered by CAAQMS", total_ncap_cities)

    ncap_cities_with_gt80_data <- ncap_city_stats %>%
      filter(percent_category == ">80% data")
    add_stat("NCAP cities with >80% data", nrow(ncap_cities_with_gt80_data))

    ncap_cities_with_lt80_data <- ncap_city_stats %>%
      filter(percent_category == "<80% data" & percent_complete > 0.01)
    add_stat("NCAP cities with <80% data but >1% data", nrow(ncap_cities_with_lt80_data))

    ncap_cities_with_no_data <- ncap_city_stats %>%
      filter(percent_category == "No data")
    add_stat("NCAP cities with <1% data", nrow(ncap_cities_with_no_data))
  })

  add_header("Non-NCAP cities")
  local({
    ncap_cities <- location_presets %>%
      filter(name == "ncap_cities")

    non_ncap_city_stats <- station_statuses %>%
      filter(!(city_id %in% ncap_cities$location_id))
    
    non_ncap_city_stats <- non_ncap_city_stats %>%
      group_by(city_id) %>%
      summarise(
        percent_complete = max(percent_complete)
      ) %>%
      mutate(
        percent_category = percent_categoriser(percent_complete)
      )
    
    total_non_ncap_cities <- nrow(non_ncap_city_stats)
    add_stat("Non-NCAP cities covered by CAAQMS", total_non_ncap_cities)

    non_ncap_cities_with_gt80_data <- non_ncap_city_stats %>%
      filter(percent_category == ">80% data")
    add_stat("Non-NCAP cities with >80% data", nrow(non_ncap_cities_with_gt80_data))

    non_ncap_cities_with_lt80_data <- non_ncap_city_stats %>%
      filter(percent_category == "<80% data" & percent_complete > 0.01)
    add_stat("Non-NCAP cities with <80% data but >1% data", nrow(non_ncap_cities_with_lt80_data))

    non_ncap_cities_with_no_data <- non_ncap_city_stats %>%
      filter(percent_category == "No data")
    add_stat("Non-NCAP cities with <1% data", nrow(non_ncap_cities_with_no_data))
  })
  

  return(key_stats)
}
