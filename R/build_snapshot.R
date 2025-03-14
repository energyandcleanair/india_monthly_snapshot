#' @importFrom logger log_threshold
#' @importFrom logger DEBUG
log_threshold(DEBUG)

#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr distinct
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr pull
#' @importFrom dplyr n_distinct
#' @importFrom logger log_info
#' @importFrom logger log_debug
#' @export
build_snapshot <- function(
    focus_month = NULL,
    output_dir = "data") {
  log_info("Initialising arguments")
  focus_month <- local({
    if (!is.null(focus_month)) {
      log_info("Using focus_month from argument")
      if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", focus_month)) {
        stop("focus_month must be in YYYY-MM-DD format")
      }
      return(
        lubridate::floor_date(
          lubridate::ymd(focus_month),
          "month"
        )
      )
    } else {
      log_info("Using focus_month from today")
      today <- lubridate::today()
      start_of_this_month <- lubridate::floor_date(today, "month")
      start_of_last_month <- lubridate::floor_date(
        start_of_this_month - lubridate::days(1),
        "month"
      )

      return(start_of_last_month)
    }
  })
  log_info("Focus month is {focus_month}")

  init_dirs(
    output_dir = output_dir,
    month_subdir = format(focus_month, "%Y-%m")
  )

  focus_month_start <- lubridate::floor_date(focus_month, "month")
  focus_month_end <- lubridate::ceiling_date(focus_month, "month") - lubridate::day(1)
  focus_year <- lubridate::year(focus_month)

  previous_month_start <- lubridate::floor_date(focus_month_start - lubridate::day(1), "month")
  log_debug(
    paste(
      "Arguments initialised: focus_month = {focus_month},",
      "focus_month_start = {focus_month_start},",
      "focus_month_end = {focus_month_end},",
      "previous_month_start = {previous_month_start}"
    )
  )

  # Get the data
  log_info("Fetching data")
  log_debug("Fetching measurements data")
  city_measurements_raw <- fetch_city_measurements_for_india(
    start_date = focus_month_start,
    end_date = focus_month_end
  )

  log_debug("Fetching station measurements data")
  station_measurements <- fetch_station_measurements_for_india(
    start_date = focus_month_start,
    end_date = focus_month_end
  ) %>%
    select(
      date,
      process_id,
      location_id,
      value,
      unit,
      city_id,
      city_name,
      gadm1_id,
      gadm1_name
    )

  log_debug("Fetching location presets data")
  location_presets <- fetch_location_presets_for_india() %>%
    select(
      name,
      location_id
    )

  station_statuses <- local({
    clean_stations <- function(stations) {
      stations %>%
        unnest_json_columns("infos") %>%
        select(id, name, city_id, city_name, latest_data, status)
    }

    log_debug("Fetching current stations data")
    stations <- fetch_current_stations_for_india() %>%
      clean_stations()

    log_debug("Fetching previous stations data")
    stations_previous <- fetch_previous_stations_for_india(
      year_month = format(previous_month_start, "%Y-%m")
    ) %>%
      clean_stations()

    log_debug("Calculating station statuses")
    station_status <- get_statuses_of_stations(
      old_stations = stations_previous,
      new_stations = stations,
      history = station_measurements,
      month = focus_month
    )

    if (nrow(stations) != nrow(station_status)) {
      stop(
        glue(
          "Mismatch in number of stations: {nrow(stations)} != {nrow(station_status)}"
        )
      )
    }

    return(station_status)
  })

  # You can add warnings to the warnings tibble to be written to the CSV at the end
  warnings <- Warnings$new()

  # Check the data
  log_info("Checking data")
  days_in_month <- focus_month_end - focus_month_start + 1
  day_threshold <- day_threshold_percent * days_in_month

  check_data(
    warnings = warnings,
    city_measurements = city_measurements_raw,
    station_measurements = station_measurements,
    location_presets = location_presets,
    day_threshold = day_threshold,
    focus_month = focus_month
  )

  valid_cities <- city_measurements_raw %>%
    group_by(city_id) %>%
    summarise(days_with_data = n_distinct(date)) %>%
    filter(days_with_data >= day_threshold) %>%
    pull(city_id)

  city_measurements <- city_measurements_raw %>%
    filter(city_id %in% valid_cities)

  city_measurements_previous_years <- lapply((focus_year - 8):(focus_year - 1), function(year) {
    log_info(paste("Fetching data for year", year))
    city_measurements_previous_year_raw <- fetch_city_measurements_for_india(
      start_date = focus_month_start %>%
        lubridate::`year<-`(year),
      end_date = focus_month_end %>%
        lubridate::`year<-`(year) %>%
        lubridate::`day<-`(lubridate::days_in_month(.)),
      use_cache = FALSE
    )

    valid_cities_previous_year <- city_measurements_previous_year_raw %>%
      group_by(city_id) %>%
      summarise(days_with_data = n_distinct(date)) %>%
      filter(days_with_data >= day_threshold) %>%
      pull(city_id)

    city_measurements_previous_year_raw %>%
      filter(city_id %in% valid_cities_previous_year)
  }) %>%
    bind_rows()

  # Generate the charts and CSVs
  log_info("Generating charts and CSVs")

  summarise_station_and_city_statuses(
    station_statuses = station_statuses,
    location_presets = location_presets,
    warnings = warnings
  ) %>%
    writeLines(file.path(get_dir("output"), "statuses_summary.md"))

  write.csv(
    station_statuses,
    file.path(get_dir("output"), "statuses.csv"),
    row.names = FALSE
  )

  analysis(
    city_measurements = city_measurements,
    city_measurements_previous_years = city_measurements_previous_years,
    station_measurements = station_measurements,
    location_presets = location_presets,
    focus_month = focus_month,
    days_in_month = days_in_month,
    warnings = warnings
  )

  # Write the warnings to a CSV
  log_info("Writing warnings to CSV")
  write.csv(
    warnings$get_warnings(),
    file.path(get_dir("output"), "warnings.csv"),
    row.names = FALSE
  )
}
