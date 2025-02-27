library(testthat)
library(mockery)

today <- as.Date("2023-01-01T00:00:00Z")
yesterday <- today - lubridate::days(1)
one_week_ago <- today - lubridate::days(7)

month_start <- today
month_end <- lubridate::ceiling_date(today, "month") - lubridate::days(1)

#' Builds history for the station for the month based on the percentage given.
#' Fills from the start date to the end date until the the last day before
#' the percentage is reached.
station_history_builder <- function(
    id,
    percentage) {
  start_date <- month_start
  end_date <- month_end

  days_to_fill <- floor((end_date - start_date + 1) * percentage)

  dates <- seq(start_date, by = "day", length.out = days_to_fill)

  if (length(dates) == 0) {
    return(tibble::tibble(
      id = character(),
      date = Sys.Date(),
      value = numeric()
    ))
  } else {
    tibble::tibble(
      id = id,
      date = dates,
      value = 100
    )
  }
}

no_history <- tibble::tibble(
  id = character(),
  date = Sys.Date(),
  value = numeric()
)

stations_none <- list(
  stations = tibble::tibble(
    id = character(),
    name = character(),
    city_id = character(),
    latest_data = date(),
    status = character()
  ),
  history = no_history
)

stations_all_live <- list(
  stations = tibble::tibble(
    id = c("1", "2", "3"),
    name = c("Station 1", "Station 2", "Station 3"),
    city_id = c("1", "2", "3"),
    latest_data = c(yesterday, yesterday, yesterday),
    status = c("Live", "Live", "Live")
  ),
  history = dplyr::bind_rows(
    station_history_builder("1", 1),
    station_history_builder("2", 1),
    station_history_builder("3", 1)
  )
)

stations_one_delayed <- list(
  stations = tibble::tibble(
    id = c("1", "2", "3"),
    name = c("Station 1", "Station 2", "Station 3"),
    city_id = c("1", "2", "3"),
    latest_data = as.Date(c(one_week_ago, yesterday, yesterday)),
    status = c("delay", "Live", "Live")
  ),
  history = dplyr::bind_rows(
    station_history_builder("1", 1 - 7 / 31),
    station_history_builder("2", 1),
    station_history_builder("3", 1)
  )
)

stations_one_inactive <- list(
  stations = tibble::tibble(
    id = c("1", "2", "3"),
    name = c("Station 1", "Station 2", "Station 3"),
    city_id = c("1", "2", "3"),
    latest_data = c(one_week_ago, yesterday, yesterday),
    status = c("Inactive", "Live", "Live")
  ),
  history = dplyr::bind_rows(
    station_history_builder("1", 0),
    station_history_builder("2", 1),
    station_history_builder("3", 1)
  )
)

stations_one_na_details <- list(
  stations = tibble::tibble(
    id = c("1", "2", "3"),
    name = c("Station 1", "Station 2", "Station 3"),
    city_id = c("1", "2", "3"),
    latest_data = c(NA, yesterday, yesterday),
    status = c(NA, "Live", "Live")
  ),
  history = dplyr::bind_rows(
    station_history_builder("2", 1),
    station_history_builder("3", 1)
  )
)

stations_one_no_record_at_all <- list(
  stations = tibble::tibble(
    id = c("2", "3"),
    name = c("Station 2", "Station 3"),
    city_id = c("2", "3"),
    latest_data = c(yesterday, yesterday),
    status = c("Live", "Live")
  ),
  history = dplyr::bind_rows(
    station_history_builder("2", 1),
    station_history_builder("3", 1)
  )
)

stations_with_various_percentage_history <- list(
  stations = tibble::tibble(
    id = c("1", "2", "3"),
    name = c("Station 1", "Station 2", "Station 3"),
    city_id = c("1", "2", "3"),
    latest_data = c(yesterday, yesterday, yesterday),
    status = c("Live", "Live", "Live")
  ),
  history = dplyr::bind_rows(
    no_history,
    station_history_builder("2", 0.75),
    station_history_builder("3", 1)
  )
)

expect_change_equal <- function(expected, actual) {
  cleaned_expected <- expected %>%
    select(id, change) %>%
    dplyr::arrange(id)
  cleaned_actual <- actual %>%
    select(id, change) %>%
    dplyr::arrange(id)

  comparison <- waldo::compare(
    cleaned_expected,
    cleaned_actual,
    x_arg = "expected",
    y_arg = "actual"
  )

  if (length(comparison) > 0) {
    testthat::fail(
      paste0(
        "Expected change tibbles to be equivalent:\n",
        paste(comparison, collapse = "\n")
      )
    )
  }
}

expect_percent_equal <- function(expected, actual) {
  cleaned_expected <- expected %>%
    select(id, percent_complete, percent_category) %>%
    dplyr::arrange(id)
  cleaned_actual <- actual %>%
    select(id, percent_complete, percent_category) %>%
    dplyr::arrange(id)

  comparison <- waldo::compare(
    cleaned_expected,
    cleaned_actual,
    x_arg = "expected",
    y_arg = "actual",
    tolerance = 0.001
  )

  if (length(comparison) > 0) {
    testthat::fail(
      paste0(
        "Expected percent tibbles to be equivalent:\n",
        paste(comparison, collapse = "\n")
      )
    )
  }
}

describe("get_statuses_of_stations", {
  describe("when both empty", {
    it("should return an empty change list", {
      diff <- indiasnapshots:::get_statuses_of_stations(
        old_stations = stations_none$stations,
        new_stations = stations_none$stations,
        history = stations_none$history,
        month = month_start
      )

      expect_equal(nrow(diff), 0)
    })
  })

  describe("for one station out of the list", {

    describe("old status \"Live\",", {
      describe("new status \"Live\"", {
        it("should return \"No change\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_all_live$stations,
            new_stations = stations_all_live$stations,
            history = stations_all_live$history,
            month = month_start
          )

          expected_stations <- stations_all_live$stations %>%
            mutate(change = case_when(id == "1" ~ "No change", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Delay\"", {
        it("should return \"No change\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_all_live$stations,
            new_stations = stations_one_delayed$stations,
            history = stations_one_delayed$history,
            month = month_start
          )

          expected_stations <- stations_one_delayed$stations %>%
            mutate(change = case_when(id == "1" ~ "No change", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Inactive\"", {
        it("should return \"Removed this month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_all_live$stations,
            new_stations = stations_one_inactive$stations,
            history = stations_one_inactive$history,
            month = month_start
          )

          expected_stations <- stations_one_inactive$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed this month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"NA details\"", {
        it("should return \"Removed this month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_all_live$stations,
            new_stations = stations_one_na_details$stations,
            history = stations_one_na_details$history,
            month = month_start
          )

          expected_stations <- stations_one_na_details$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed this month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
    })
    describe("old status \"delay\"", {
      describe("new status \"Live\"", {
        it("should return \"No change\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_delayed$stations,
            new_stations = stations_all_live$stations,
            history = stations_all_live$history,
            month = month_start
          )

          expected_stations <- stations_all_live$stations %>%
            mutate(change = case_when(id == "1" ~ "No change", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Inactive\"", {
        it("should return \"Removed this month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_delayed$stations,
            new_stations = stations_one_inactive$stations,
            history = stations_one_inactive$history,
            month = month_start
          )

          expected_stations <- stations_one_inactive$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed this month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"NA details\"", {
        it("should return \"Removed this month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_delayed$stations,
            new_stations = stations_one_na_details$stations,
            history = stations_one_na_details$history,
            month = month_start
          )

          expected_stations <- stations_one_na_details$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed this month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
    })
    describe("old status \"Inactive\"", {
      # Inactive -> Live = Reactivated
      # Inactive -> Delay = Reactivated
      # Inactive -> Inactive = Removed in a previous month
      # Inactive -> NA details = Removed in a previous month
      describe("new status \"Live\"", {
        it("should return \"Reactivated\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_inactive$stations,
            new_stations = stations_all_live$stations,
            history = stations_all_live$history,
            month = month_start
          )

          expected_stations <- stations_all_live$stations %>%
            mutate(change = case_when(id == "1" ~ "Reactivated", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Delay\"", {
        it("should return \"Reactivated\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_inactive$stations,
            new_stations = stations_one_delayed$stations,
            history = stations_one_delayed$history,
            month = month_start
          )

          expected_stations <- stations_one_delayed$stations %>%
            mutate(change = case_when(id == "1" ~ "Reactivated", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Inactive\"", {
        it("should return \"Removed in a previous month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_inactive$stations,
            new_stations = stations_one_inactive$stations,
            history = stations_one_inactive$history,
            month = month_start
          )

          expected_stations <- stations_one_inactive$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed in a previous month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"NA details\"", {
        it("should return \"Removed in a previous month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_inactive$stations,
            new_stations = stations_one_na_details$stations,
            history = stations_one_na_details$history,
            month = month_start
          )

          expected_stations <- stations_one_na_details$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed in a previous month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
    })
    describe("old status \"NA details\"", {
      # NA details -> Live = Reactivated
      # NA details -> Delay = Reactivated
      # NA details -> Inactive = Removed in a previous month
      # NA details -> NA details = Removed in a previous month
      describe("new status \"Live\"", {
        it("should return \"Reactivated\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_na_details$stations,
            new_stations = stations_all_live$stations,
            history = stations_all_live$history,
            month = month_start
          )

          expected_stations <- stations_all_live$stations %>%
            mutate(change = case_when(id == "1" ~ "Reactivated", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Delay\"", {
        it("should return \"Reactivated\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_na_details$stations,
            new_stations = stations_one_delayed$stations,
            history = stations_one_delayed$history,
            month = month_start
          )

          expected_stations <- stations_one_delayed$stations %>%
            mutate(change = case_when(id == "1" ~ "Reactivated", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Inactive\"", {
        it("should return \"Removed in a previous month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_na_details$stations,
            new_stations = stations_one_inactive$stations,
            history = stations_one_inactive$history,
            month = month_start
          )

          expected_stations <- stations_one_inactive$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed in a previous month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"NA details\"", {
        it("should return \"Removed in a previous month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_na_details$stations,
            new_stations = stations_one_na_details$stations,
            history = stations_one_na_details$history,
            month = month_start
          )

          expected_stations <- stations_one_na_details$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed in a previous month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
    })
    describe("not in old list", {
      # not in old list -> Live = New
      # not in old list -> Delay = New
      # not in old list -> Inactive = Removed in a previous month
      # not in old list -> NA details = Removed in a previous month
      describe("new status \"Live\"", {
        it("should return \"New\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_no_record_at_all$stations,
            new_stations = stations_all_live$stations,
            history = stations_all_live$history,
            month = month_start
          )

          expected_stations <- stations_all_live$stations %>%
            mutate(change = case_when(id == "1" ~ "New", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Delay\"", {
        it("should return \"New\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_no_record_at_all$stations,
            new_stations = stations_one_delayed$stations,
            history = stations_one_delayed$history,
            month = month_start
          )

          expected_stations <- stations_one_delayed$stations %>%
            mutate(change = case_when(id == "1" ~ "New", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"Inactive\"", {
        it("should return \"Removed in a previous month\"", {
          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_no_record_at_all$stations,
            new_stations = stations_one_inactive$stations,
            history = stations_one_inactive$history,
            month = month_start
          )

          expected_stations <- stations_one_inactive$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed in a previous month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
      describe("new status \"NA details\"", {
        it("should return \"Removed in a previous month\"", {
          # Mock lubridate::today() to return a fixed date
          stub(indiasnapshots:::get_statuses_of_stations, "lubridate::today", today)

          diff <- indiasnapshots:::get_statuses_of_stations(
            old_stations = stations_one_no_record_at_all$stations,
            new_stations = stations_one_na_details$stations,
            history = stations_one_na_details$history,
            month = month_start
          )

          expected_stations <- stations_one_na_details$stations %>%
            mutate(change = case_when(id == "1" ~ "Removed in a previous month", TRUE ~ "No change"))

          expect_change_equal(expected_stations, diff)
        })
      })
    })
  })

  describe("with a variety of history", {
    it("should return the correct percentages and categories", {
      diff <- indiasnapshots:::get_statuses_of_stations(
        old_stations = stations_all_live$stations,
        new_stations = stations_with_various_percentage_history$stations,
        history = stations_with_various_percentage_history$history,
        month = month_start
      )

      expected_stations <- stations_with_various_percentage_history$stations %>%
        mutate(percent_complete = case_when(
          id == "1" ~ 0.0,
          id == "2" ~ 0.7419, # 23 days out of 31, smallest number of days < 75%
          id == "3" ~ 1.0
        )) %>%
        mutate(percent_category = case_when(
          id == "1" ~ "No data",
          id == "2" ~ "<80% data",
          id == "3" ~ ">80% data"
        ))

      expect_percent_equal(expected_stations, diff)
    })
  })

})
