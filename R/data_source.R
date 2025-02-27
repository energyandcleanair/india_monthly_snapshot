cache_data <- function(url, cache_file) {
  if (!file.exists(cache_file)) {
    data <- readr::read_csv(url, show_col_types = FALSE)
    write.csv(data, cache_file, row.names = FALSE)
  }
  return(readr::read_csv(cache_file, show_col_types = FALSE))
}
fetch_data <- function(url, cache_file, use_cache = TRUE) {
  if (use_cache) {
    return(cache_data(url, cache_file))
  } else {
    return(readr::read_csv(url, show_col_types = FALSE))
  }
}

fetch_city_measurements_for_india <- function(start_date, end_date, ..., use_cache = TRUE) {
  measurements_url <- glue(
    "https://api.energyandcleanair.org/v1/measurements",
    "?format=csv",
    "&process_id=city_day_mad",
    "&date_from={start_date}",
    "&date_to={end_date}",
    "&source=cpcb",
    "&pollutant=pm25"
  )
  cache_file <- file.path(get_dir("cache"), "measurements.csv")
  return(fetch_data(measurements_url, cache_file, use_cache))
}

fetch_station_measurments_for_india <- function(start_date, end_date, ..., use_cache = TRUE) {
  measurements_url <- glue(
    "https://api.energyandcleanair.org/v1/measurements",
    "?format=csv",
    "&process_id=station_day_mad",
    "&date_from={start_date}",
    "&date_to={end_date}",
    "&source=cpcb",
    "&pollutant=pm25"
  )
  cache_file <- file.path(get_dir("cache"), "measurements_stations.csv")
  return(fetch_data(measurements_url, cache_file, use_cache))
}

fetch_current_stations_for_india <- function(..., use_cache = TRUE) {
  url <- glue("https://api.energyandcleanair.org/stations?format=csv&source=cpcb")
  cache_file <- file.path(get_dir("cache"), "stations.csv")
  return(fetch_data(url, cache_file, use_cache))
}

fetch_previous_stations_for_india <- function(year_month, ..., use_cache = TRUE) {
  url <- glue("https://storage.googleapis.com/crea-public/plots/india_snapshots/{year_month}/cache/stations.csv")
  cache_file <- file.path(get_dir("cache"), "stations_previous.csv")
  return(fetch_data(url, cache_file, use_cache))
}

fetch_location_presets_for_india <- function(year_month, ..., use_cache = TRUE) {
  url <- "http://api.energyandcleanair.org/v1/location_presets?format=csv"
  cache_file <- file.path(get_dir("cache"), "location_presets.csv")
  return(fetch_data(url, cache_file, use_cache))
}
