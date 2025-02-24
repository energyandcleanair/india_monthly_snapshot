
fetch_measurements_for_india <- function(start_date, end_date, ..., use_cache = TRUE) {

  measurements_url <- glue(
    "https://api.energyandcleanair.org/v1/measurements",
    "?format=csv",
    "&process_id=city_day_mad",
    "&date_from={start_date}",
    "&date_to={end_date}",
    "&source=cpcb",
    "&pollutant=pm25"
  )

  if (use_cache) {

    cache_file <- file.path(
      get_dir("cache"),
      glue("measurements.csv")
    )

    if (!file.exists(cache_file)) {
      measurements <- readr::read_csv(measurements_url, show_col_types = FALSE)
      write.csv(measurements, cache_file, row.names = FALSE)
    }

    return(readr::read_csv(cache_file, show_col_types = FALSE))
  } else {
    return(readr::read_csv(measurements_url, show_col_types = FALSE))
  }
}
