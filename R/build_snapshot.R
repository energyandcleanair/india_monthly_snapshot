
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export
build_snapshot <- function(
    focus_month = NULL,
    output_dir = "output") {
  
  focus_month <- local({
    if (!is.null(focus_month)) {
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
      today <- lubridate::today()
      start_of_this_month <- lubridate::floor_date(today, "month")
      start_of_last_month <- lubridate::floor_date(
        start_of_this_month - lubridate::days(1),
        "month"
      )

      return(start_of_last_month)
    }
  })

  init_dirs(format(focus_month, "%Y-%m"))

  focus_month_start <- lubridate::floor_date(focus_month, "month")
  focus_month_end <- lubridate::ceiling_date(focus_month, "month") - lubridate::day(1)

  previous_month_start <- lubridate::floor_date(focus_month_start - lubridate::day(1), "month")

  # Get the data
  measurements <- fetch_city_measurements_for_india(
    start_date = focus_month_start,
    end_date = focus_month_end
  )

  station_measurements <- fetch_station_measurments_for_india(
    start_date = focus_month_start,
    end_date = focus_month_end
  )

  location_presets <- fetch_location_presets_for_india()

  clean_stations <- function(stations) {
    stations %>%
      mutate(infos = gsub("'", "\"", infos)) %>%
      mutate(infos = gsub("None", "null", infos)) %>%
      mutate(infos = ifelse(is.na(infos), "{}", infos)) %>%
      mutate(infos = lapply(infos, jsonlite::fromJSON)) %>%
      tidyr::unnest_wider(infos) %>%
      select(id, name, city_id, latest_data, status)
  }

  stations <- fetch_current_stations_for_india() %>%
    clean_stations()

  stations_previous <- fetch_previous_stations_for_india(
    year_month = format(previous_month_start, "%Y-%m")
  ) %>%
    clean_stations()

  # Check the data

  # Generate the charts and CSVs
}
