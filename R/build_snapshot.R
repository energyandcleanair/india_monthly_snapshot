log_threshold(DEBUG)

#' @export
build_snapshot <- function(
    focus_period = NULL,
    output_dir = "data") {
  log_info("Installing fonts")
  font_add_google("Source Sans Pro", "source_sans_pro")
  showtext_auto()

  log_info("Initialising arguments")
  focus_period <- local({
    if (!is.null(focus_period)) {
      log_info("Using focus_period from argument")
      if (!grepl("^\\d{4}-\\d{2}$", focus_period) & (!grepl("^\\d{4}-H(1|2)$", focus_period))) {
        stop("focus_period must be in YYYY-MM or YYYY-Hn format")
      }
      return(focus_period)
    } else {
      log_info("Using focus_period from today")
      today <- lubridate::today()
      start_of_this_month <- lubridate::floor_date(today, "month")
      start_of_last_month <- lubridate::floor_date(
        start_of_this_month - lubridate::days(1),
        "month"
      )

      return(start_of_last_month %>% format("%Y-%m"))
    }
  })

  focus_period_mode <- if (focus_period %>% grepl("^\\d{4}-H(1|2)$", .)) {
    "half_year"
  } else {
    "month"
  }

  log_info("Focus period is {focus_period}")

  init_dirs(
    output_dir = output_dir,
    subdir = focus_period
  )

  if (focus_period_mode == "half_year") {
    focus_half_year <- focus_period %>%
      gsub("^\\d{4}-H(1|2)$", "\\1", .) %>%
      as.integer()

    focus_year <- focus_period %>%
      gsub("^(\\d{4})-H(1|2)$", "\\1", .) %>%
      as.integer()

    focus_period_start <- ymd(paste0(focus_year, "-", (focus_half_year - 1) * 6 + 1, "-01"))
    focus_period_end <- ymd(
      paste0(focus_year, "-", (focus_half_year) * 6, if (focus_half_year == 1) "-30" else "-31")
    )
  } else {
    focus_period_start <- ymd(paste0(focus_period, "-01"))
    focus_period_end <- lubridate::ceiling_date(focus_period_start, "month") - lubridate::days(1)
  }

  focus_year <- lubridate::year(focus_period_start)

  # only for stations comparison
  station_comparison_month <- lubridate::floor_date(focus_period_start - lubridate::day(1), "month")
  log_debug(
    paste(
      "Arguments initialised: ",
      "focus_period_start = {focus_period_start},",
      "focus_period_end = {focus_period_end},",
      "station_comparison_month = {station_comparison_month}"
    )
  )

  # Get the data
  log_info("Fetching data")
  log_debug("Fetching cities")
  cities <- fetch_cities_for_india() %>% select(id, longitude, latitude)

  log_debug("Fetching measurements data")
  city_measurements_raw <- fetch_city_measurements_for_india(
    start_date = focus_period_start,
    end_date = focus_period_end
  )

  log_debug("Fetching station measurements data")
  station_measurements <- fetch_station_measurements_for_india(
    start_date = focus_period_start,
    end_date = focus_period_end
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

  log_debug("Fetching overshooting dates")
  overshooting_data <- fetch_overshooting_for_india(
    start_date = paste0(focus_year, "-01-01"),
    cities = cities$id
  )

  write.csv(
    overshooting_data,
    file.path(get_dir("output"), "overshooting.csv"),
    row.names = FALSE
  )

  # Check the data
  log_info("Checking data")
  days_in_analysis <- as.integer(focus_period_end - focus_period_start + 1)
  day_threshold <- day_threshold_percent * days_in_analysis

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
      year_month = format(station_comparison_month, "%Y-%m")
    ) %>%
      clean_stations()

    log_debug("Calculating station statuses")
    station_status <- get_statuses_of_stations(
      old_stations = stations_previous,
      new_stations = stations,
      history = station_measurements,
      days_in_analysis = days_in_analysis
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


  check_data(
    warnings = warnings,
    city_measurements = city_measurements_raw,
    station_measurements = station_measurements,
    location_presets = location_presets,
    day_threshold = day_threshold
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
      start_date = focus_period_start %>%
        lubridate::`year<-`(year),
      end_date = focus_period_end %>%
        lubridate::`year<-`(year) %>%
        lubridate::`day<-`(lubridate::days_in_month(.))
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

  chart_date_subtitle <- if (focus_period_mode == "half_year") {
    paste0(
      format(focus_period_start, "%B"),
      " to ",
      format(focus_period_end, "%B %Y")
    )
  } else {
    paste0(
      format(focus_period_start, "%B %Y")
    )
  }

  analysis(
    cities = cities,
    city_measurements = city_measurements,
    city_measurements_previous_years = city_measurements_previous_years,
    station_measurements = station_measurements,
    location_presets = location_presets,
    chart_date_subtitle = chart_date_subtitle,
    focus_year = focus_year,
    focus_period_mode = focus_period_mode,
    focus_period_start = focus_period_start,
    days_in_analysis = days_in_analysis,
    warnings = warnings
  )

  # Write the warnings to a CSV
  log_info("Writing warnings to CSV")
  write.csv(
    warnings$get_warnings(),
    file.path(get_dir("output"), "warnings.csv"),
    row.names = FALSE
  )

  log_info("Create a zip of the output directory")
  # Create a zip file of the output directory
  zip_file <- file.path(get_dir("month"), "results.zip")
  if (file.exists(zip_file)) {
    file.remove(zip_file)
  }
  zip::zipr(
    zip_file,
    files = get_dir("month"),
    recurse = TRUE
  )
  log_info("Zip file created at {zip_file}")

}
