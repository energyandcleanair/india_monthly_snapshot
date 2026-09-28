library(testthat)

make_edition <- function(period = "2026-08") {
  output_dir <- withr::local_tempdir(.local_envir = parent.frame())
  period_dir <- file.path(output_dir, period)
  dir.create(file.path(period_dir, "output"), recursive = TRUE)
  dir.create(file.path(period_dir, "cache"))
  output <- function(name) file.path(period_dir, "output", name)

  plot <- ggplot2::ggplot() +
    ggplot2::labs(title = "Top 10 most polluted cities \n- August 2026")
  file.create(output("top10_polluted_cities.png"))
  write.csv(data.frame(city_name = "Delhi", mean = 80), output("top10_polluted_cities.csv"))
  write_chart_companions(
    file = output("top10_polluted_cities.png"), plot = plot,
    data_through = as.Date(c("2026-08-30", "2026-08-31"))
  )

  file.create(output("pm25_calendar_Delhi.png"))
  write_chart_companions(
    file = output("pm25_calendar_Delhi.png"),
    data = data.frame(date = as.Date("2026-08-31"), city_name = "Delhi", value = 50),
    data_through = as.Date("2026-08-31"),
    title = "Delhi daily PM2.5 (µg/m³)"
  )

  file.create(output("mystery.png"))

  write.csv(data.frame(name = "ncap_cities", total = 3), output("monthly_compliance.csv"))
  write_table_companion(output("monthly_compliance.csv"), title = "Compliance")

  write.csv(
    data.frame(
      id = paste0("s", 1:5),
      name = paste("Station", 1:5),
      city_id = c("delhi", "delhi", "agra", "pune", "new_town"),
      city_name = c("Delhi", "Delhi", "Agra", "Pune", "New Town"),
      change = c("No change", "No change", "Removed this month", "Reactivated", "New"),
      percent_complete = c(1, 0.5, 0, 0.9, 0.3),
      percent_category = c(">80% data", "<80% data", "No data", ">80% data", "<80% data")
    ),
    output("statuses.csv"),
    row.names = FALSE
  )
  write.csv(
    data.frame(name = "ncap_cities", location_id = c("delhi", "agra")),
    file.path(period_dir, "cache", "location_presets.csv"),
    row.names = FALSE
  )
  write.csv(
    snapshot_input("City PM2.5 (CPCB)", as.Date(c("2026-08-01", "2026-08-31"))),
    file.path(period_dir, "data_summary.csv"),
    row.names = FALSE
  )
  output_dir
}

test_that("site.json lists every chart and table by section", {
  output_dir <- make_edition()
  build_snapshot_site(output_dir, "2026-08")
  site <- jsonlite::read_json(file.path(output_dir, "2026-08", "site.json"))

  expect_equal(site$focus_month, "2026-08")
  expect_null(site$edition_label)
  cards <- setNames(site$cards, vapply(site$cards, `[[`, "", "key"))
  expect_setequal(
    names(cards),
    c("top10_polluted_cities", "pm25_calendar_Delhi", "mystery", "monthly_compliance")
  )

  top10 <- cards$top10_polluted_cities
  expect_equal(top10$section, "rankings")
  expect_equal(top10$files$all$png, "output/top10_polluted_cities.png")
  expect_equal(top10$files$all$csv, "output/top10_polluted_cities.csv")
  expect_equal(top10$files$all$meta$title, "Top 10 most polluted cities - August 2026")
  expect_equal(top10$files$all$meta$data_through, "2026-08-31")

  expect_equal(cards$pm25_calendar_Delhi$section, "cities")
  expect_equal(cards$pm25_calendar_Delhi$files$all$meta$title, "Delhi daily PM2.5 (µg/m³)")
  expect_equal(cards$pm25_calendar_Delhi$files$all$csv, "output/pm25_calendar_Delhi.csv")
  expect_equal(cards$mystery$section, "other")
  expect_null(cards$mystery$files$all$meta)

  table <- cards$monthly_compliance
  expect_equal(table$kind, "table")
  expect_equal(table$section, "compliance")
  expect_null(table$files$all$png)

  expect_equal(site$inputs[[1]]$latest_data, "2026-08-31")
  expect_equal(site$sections[[length(site$sections)]]$id, "other")
})

test_that("coverage counts stations and cities by share of days with data", {
  output_dir <- make_edition()
  coverage <- build_snapshot_site(output_dir, "2026-08")$coverage
  groups <- setNames(coverage$groups, vapply(coverage$groups, `[[`, "", "id"))

  expect_equal(groups$stations[c("total", "gt80", "mid", "lt1")],
               list(total = 5L, gt80 = 2L, mid = 2L, lt1 = 1L))
  # a city takes its best station
  expect_equal(groups$cities[c("total", "gt80", "mid", "lt1")],
               list(total = 4L, gt80 = 2L, mid = 1L, lt1 = 1L))
  expect_equal(groups$ncap[c("total", "gt80", "lt1")], list(total = 2L, gt80 = 1L, lt1 = 1L))
  expect_equal(groups$non_ncap$total, 2L)

  expect_equal(as.character(coverage$changes$new), "Station 5")
  expect_equal(as.character(coverage$changes$removed), "Station 3")
  expect_equal(as.character(coverage$changes$new_cities), "New Town")
})

test_that("latest.json follows monthly editions only", {
  output_dir <- make_edition("2026-08")
  build_snapshot_site(output_dir, "2026-08")
  latest <- jsonlite::read_json(file.path(output_dir, "latest.json"))
  expect_equal(latest, list(month_dir = "2026-08", focus_month = "2026-08"))

  half_dir <- make_edition("2025-H2")
  site <- build_snapshot_site(half_dir, "2025-H2")
  expect_equal(site$focus_month, "2025-12")
  expect_equal(site$edition_label, "H2 2025")
  expect_false(file.exists(file.path(half_dir, "latest.json")))

  build_snapshot_site(output_dir, "2026-08", update_latest = FALSE)
  expect_true(file.exists(file.path(output_dir, "latest.json")))
})

test_that("an edition written before the catalog still gets a site.json", {
  output_dir <- make_edition()
  unlink(list.files(file.path(output_dir, "2026-08", "output"), "\\.json$", full.names = TRUE))
  site <- snapshot_site_catalog(file.path(output_dir, "2026-08"))
  expect_length(site$cards, 3)
  expect_null(site$cards[[1]]$files$all$meta)
})
