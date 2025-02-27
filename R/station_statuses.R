#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr join_by
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr n
get_statuses_of_stations <- function(..., old_stations, new_stations, history, month) {
  active_statuses <- c("live", "delay")

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
      latest_data = latest_data_new,
      exists_old,
      exists_new,
      status_old,
      status_new,
    ) %>%
    mutate(
      status_old = stringr::str_to_lower(status_old),
      status_new = stringr::str_to_lower(status_new),
      is_new = is.na(exists_old) & (status_new %in% active_statuses),
      no_change = status_old %in% active_statuses &
        status_new %in% active_statuses,
      reactivated = (is.na(status_old) | status_old == "inactive") &
        status_new %in% active_statuses,
      removed_this_month = status_old %in% active_statuses &
        (is.na(status_new) | status_new == "inactive"),
      removed_previous_month = (status_old == "inactive" | is.na(status_old) | !exists_old) &
        (is.na(status_new) | status_new == "inactive"),
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
      latest_data,
      status = status_new,
      change
    )

  percentages <- history %>%
    group_by(id) %>%
    summarise(
      percent_complete = n() / as.numeric(lubridate::days_in_month(month))
    ) %>%
    mutate(
      percent_category = case_when(
        percent_complete < 0.01 ~ "No data",
        percent_complete < 0.8 ~ "<80% data",
        percent_complete >= 0.8 ~ ">80% data",
        TRUE ~ "Unknown"
      )
    )

  results <- status %>%
    left_join(percentages, by = "id") %>%
    mutate(
      percent_complete = ifelse(is.na(percent_complete), 0, percent_complete),
      percent_category = ifelse(is.na(percent_category), "No data", percent_category)
    )

  return(results)
}
