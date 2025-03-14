fetch_city_measurements_for_india <- function(
    start_date,
    end_date,
    ...,
    use_cache = TRUE) {
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

  city_measurement_rules <- validate::validator(
    all(level == "city"),
    all(process_id == "city_day_mad")
  )
  return(
    fetch_data(measurements_url, cache_file, use_cache) %>%
      validate(
        columns = measurement_columns,
        rules = measurement_rules + city_measurement_rules
      )
  )
}

fetch_station_measurements_for_india <- function(start_date, end_date, ..., use_cache = TRUE) {
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

  station_measurement_rules <- validate::validator(
    all(level == "station"),
    all(process_id == "station_day_mad")
  )

  return(
    fetch_data(measurements_url, cache_file, use_cache) %>%
      validate(
        columns = measurement_columns,
        rules = measurement_rules + station_measurement_rules
      )
  )
}

fetch_current_stations_for_india <- function(..., use_cache = TRUE) {
  url <- glue(
    "https://api.energyandcleanair.org/stations",
    "?format=csv",
    "&source=cpcb",
    "&with_data_only=false"
  )
  cache_file <- file.path(get_dir("cache"), "stations.csv")
  return(
    fetch_data(url, cache_file, use_cache) %>%
      validate(
        columns = station_columns,
        rules = station_rules
      )
  )
}

fetch_previous_stations_for_india <- function(year_month, ..., use_cache = TRUE) {
  url <- glue(
    "https://storage.googleapis.com/",
    "crea-public/plots/india_snapshots/{year_month}/cache/stations.csv"
  )
  cache_file <- file.path(get_dir("cache"), "stations_previous.csv")
  return(
    fetch_data(url, cache_file, use_cache) %>%
      validate(
        columns = station_columns,
        rules = station_rules
      )
  )
}

fetch_location_presets_for_india <- function(year_month, ..., use_cache = TRUE) {
  url <- glue(
    "http://api.energyandcleanair.org/v1/location_presets",
    "?format=csv"
  )
  cache_file <- file.path(get_dir("cache"), "location_presets.csv")
  return(
    fetch_data(url, cache_file, use_cache) %>%
      validate(
        columns = c("name", "location_id"),
        rules = validate::validator(
          is.character(name),
          is.character(location_id)
        )
      )
  )
}

fetch_cities_for_india <- function() {
  url <- glue(
    "https://api.energyandcleanair.org/cities",
    "?country=IN&format=csv"
  )
  cache_file <- file.path(get_dir("cache"), "cities.csv")
  return(
    fetch_data(url, cache_file) %>%
      validate(
        columns = c("city_id", "city_name", "gadm1_id", "gadm1_name"),
        rules = validate::validator(
          is.character(city_id),
          is.character(city_name),
          is.character(gadm1_id),
          is.character(gadm1_name)
        )
      )
  )
}

measurement_columns <- c(
  "date",
  "process_id",
  "location_id",
  "variable",
  "value",
  "unit",
  "source",
  "pollutant",
  "pollutant_name",
  "country_id",
  "level",
  "city_id",
  "city_name",
  "gadm1_name",
  "gadm1_id"
)

measurement_rules <- validate::validator(
  all(is.Date(date)),
  all(variable == "observed"),
  is.numeric(value),
  all(source == "cpcb"),
  all(unit == "µg/m3"),
  is.character(location_id),
  all(pollutant == "pm25"),
  all(pollutant_name == "PM2.5"),
  all(country_id == "IN"),
  is.character(level),
  is.character(city_id),
  is.character(city_name),
  is.character(gadm1_name),
  is.character(gadm1_id)
)

station_columns <- c(
  "id", "name", "city_id", "city_name", "infos", "source", "country_id", "level"
)

station_rules <- validate::validator(
  is.character(id),
  is.character(name),
  is.character(city_id),
  is.character(city_name),
  is.character(infos),
  all(source == "cpcb"),
  all(country_id == "IN"),
  all(level == "station")
)

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
validate <- function(data, ..., columns, rules) {
  if (!missing(columns)) {
    missing_columns <- setdiff(columns, colnames(data))
    if (length(missing_columns) > 0) {
      stop(paste("Columns missing in data:", missing_columns))
    }
  }

  if (!missing(rules)) {
    summary_out <- validate::confront(data, rules, raise = "errors")
  }

  return(data)
}
