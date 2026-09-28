
get_statuses_of_stations <- function(..., old_stations, new_stations, history, days_in_analysis) {
  active_statuses <- c("live", "delay")

  log_debug("Calculating station changes")
  status <- full_join(
    old_stations %>% mutate(exists = TRUE),
    new_stations %>% mutate(exists = TRUE),
    by = join_by(
      "id"
    ),
    suffix = c("_old", "_new")
  ) %>%
    select(
      id,
      name = name_new,
      city_id = city_id_new,
      city_name = city_name_new,
      latest_data = latest_data_new,
      exists_old,
      exists_new,
      status_old,
      status_new,
    ) %>%
    mutate(
      # These are a couple of helper variables to make the statements easier to read
      status_old = stringr::str_to_lower(status_old),
      status_new = stringr::str_to_lower(status_new),
      missing_status_old = is.na(status_old),
      missing_status_new = is.na(status_new),
      # Booleans for the status of the station old and now
      exists_old = !is.na(exists_old),
      exists_new = !is.na(exists_new),
      old_active = status_old %in% active_statuses,
      new_active = status_new %in% active_statuses,
      old_inactive = missing_status_old | status_old == "inactive",
      new_inactive = missing_status_new | status_new == "inactive",
      # Change type (as a boolean for each)
      is_new = !exists_old & new_active,
      no_change = (old_active & new_active) | (!exists_old & new_inactive),
      reactivated = old_inactive & new_active,
      removed_this_month = old_active & new_inactive,
      removed_previous_month = old_inactive & new_inactive,
      # Change type to text in a single column
      change = case_when(
        is_new ~ "New",
        no_change ~ "No change",
        reactivated ~ "Reactivated",
        removed_this_month ~ "Removed this month",
        removed_previous_month ~ "Removed in a previous month",
        .default = "Change undefined"
      )
    ) %>%
    select(
      id,
      name,
      city_id,
      city_name,
      latest_data,
      status = status_new,
      change
    )

  log_debug("Calculating station data completeness")
  percentages <- history %>%
    group_by(location_id) %>%
    summarise(
      percent_complete = n() / days_in_analysis
    ) %>%
    mutate(
      percent_category = percent_categoriser(percent_complete)
    ) %>%
    select(
      id = location_id,
      percent_complete,
      percent_category
    )


  log_debug("Joining station statuses and data completeness")
  results <- status %>%
    left_join(percentages, by = "id") %>%
    mutate(
      percent_complete = ifelse(is.na(percent_complete), 0, percent_complete),
      percent_category = ifelse(is.na(percent_category), "No data", percent_category)
    )

  return(results)
}

percent_categoriser <- function(percent_complete) {
  case_when(
    percent_complete < 0.01 ~ "No data",
    percent_complete < 0.8 ~ "<80% data",
    .default = ">80% data"
  )
}

#' How each station's reporting changed since the previous period, from its data
#'
#' `change` follows the API's live/inactive flag at run time, which flips on short gaps.
#' `coverage_change` compares the share of days with data (at least 1%) in both periods:
#' "New" (not in the previous station list), "Reactivated", "Removed" (no data this
#' period after some the last), "No change" (data in both) or "No data" (in neither).
#'
#' @param statuses from get_statuses_of_stations()
#' @param previous_station_ids stations listed at the previous run
#' @param previous_history station measurements of the previous period
#' @param previous_days days in the previous period
get_coverage_changes <- function(
    ...,
    statuses,
    previous_station_ids,
    previous_history,
    previous_days) {
  previous <- previous_history %>%
    group_by(location_id) %>%
    summarise(previous_percent_complete = n() / previous_days)

  statuses %>%
    left_join(previous, by = c("id" = "location_id")) %>%
    mutate(
      previous_percent_complete = replace_na(previous_percent_complete, 0),
      had_data = percent_categoriser(previous_percent_complete) != "No data",
      has_data = percent_categoriser(percent_complete) != "No data",
      coverage_change = case_when(
        had_data & has_data ~ "No change",
        had_data ~ "Removed",
        has_data & !(id %in% previous_station_ids) ~ "New",
        has_data ~ "Reactivated",
        .default = "No data"
      )
    ) %>%
    select(-had_data, -has_data)
}
