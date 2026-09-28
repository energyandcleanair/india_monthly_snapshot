# Snapshot site catalog, read by the India portal's "Air quality snapshot" page.
#
# Next to every chart in output/ we write <stem>.json (title, date the data reaches) and,
# when it had none, <stem>.csv (the data behind it). At the end of a run,
# <period>/site.json lists every chart by section, with the data sources' latest dates
# and the station coverage; <output_dir>/latest.json points at the newest monthly edition.
#
# The catalog is built from the files on disk only, so it can be rebuilt for any past
# edition. Same layout as the China snapshot (china_co2 R/viz_snapshot_site.R).

snapshot_sections <- list(
  list(
    id = "compliance", title = "Compliance with standards",
    blurb = "Cities meeting the NAAQS and WHO PM2.5 standards, and their GRAP categories.",
    pattern = "^(compliance|monthly_compliance|cities_grap_distribution)$"
  ),
  list(
    id = "rankings", title = "City rankings",
    blurb = "The most and least polluted cities, and how they compare with a year earlier.",
    pattern = "^(top10_|all_cities_ordered$)"
  ),
  list(
    id = "regional", title = "States and regions",
    blurb = "The most polluted city in each state, state capitals and the Indo-Gangetic Plain.",
    pattern = "^(top_city_province|state_capitals|igp_)"
  ),
  list(
    id = "cities", title = "Major cities",
    blurb = "Daily PM2.5 calendars.",
    pattern = "^pm25_calendar_"
  )
)

#' Save a chart with rcrea::quicksave(), plus its companion files
#'
#' @param data the frame behind the chart, written as <stem>.csv
#' @param data_through last date the chart's data reaches
#' @param title shown on the site; defaults to the plot's title
save_snapshot_chart <- function(file, plot, ..., data = NULL, data_through = NULL, title = NULL) {
  rcrea::quicksave(file, plot = plot, ...)
  write_chart_companions(
    file = file, plot = plot, data = data, data_through = data_through, title = title
  )
}

write_chart_companions <- function(
  ...,
  file,
  plot = NULL,
  data = NULL,
  data_through = NULL,
  title = NULL
) {
  if (!is.null(data)) {
    write.csv(ungroup(data), companion_path(file, "csv"), row.names = FALSE)
  }
  write_snapshot_json(
    list(
      kind = "chart",
      title = clean_label(if (!is.null(title)) title else plot_label(plot, "title")),
      subtitle = clean_label(plot_label(plot, "subtitle")),
      data_through = format_date(data_through),
      chart = NULL
    ),
    companion_path(file, "json")
  )
}

#' Show a CSV on the site as a table
write_table_companion <- function(file, title, data_through = NULL) {
  write_snapshot_json(
    list(
      kind = "table", csv = basename(file), title = title,
      data_through = format_date(data_through), chart = NULL
    ),
    companion_path(file, "json")
  )
}

companion_path <- function(file, ext) sub("\\.[^./]+$", paste0(".", ext), file)

plot_label <- function(plot, name) {
  label <- tryCatch(plot$labels[[name]], error = function(e) NULL)
  if (is.character(label) && length(label) == 1 && !is.na(label)) label else NULL
}

clean_label <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(NULL)
  }
  gsub("\\s*\n\\s*", " ", trimws(x))
}

format_date <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NULL)
  }
  format(max(as.Date(x)))
}

# UTF-8 whatever the locale: write_json() escapes µ as <U+00B5> under a C locale
write_snapshot_json <- function(x, path) {
  json <- jsonlite::toJSON(
    x,
    auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA
  )
  writeLines(enc2utf8(json), path, useBytes = TRUE)
}

#' One row per data source, with the last date it has data for
snapshot_input <- function(name, dates) {
  data.frame(file = name, latest_data = format_date(dates) %||% NA, latest_update = NA)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

snapshot_section_id <- function(stem) {
  for (section in snapshot_sections) {
    if (grepl(section$pattern, stem)) {
      return(section$id)
    }
  }
  "other"
}

#' The focus month and label of an edition folder: 2026-08, or 2025-H1 (its last month)
snapshot_period <- function(period) {
  if (grepl("^\\d{4}-\\d{2}$", period)) {
    return(list(focus_month = period, edition_label = NULL))
  }
  if (grepl("^\\d{4}-H[12]$", period)) {
    year <- substr(period, 1, 4)
    half <- substr(period, 7, 7)
    return(list(
      focus_month = paste0(year, if (half == "1") "-06" else "-12"),
      edition_label = paste0("H", half, " ", year)
    ))
  }
  stop("Not a snapshot period: ", period)
}

#' Stations and cities by share of days with data, as in statuses_summary.md
#'
#' @param station_statuses statuses.csv
#' @param location_presets cache/location_presets.csv
snapshot_coverage <- function(station_statuses, location_presets) {
  by_category <- function(percent_category) {
    list(
      total = length(percent_category),
      gt80 = sum(percent_category == ">80% data"),
      mid = sum(percent_category == "<80% data"),
      lt1 = sum(percent_category == "No data")
    )
  }
  cities <- station_statuses %>%
    group_by(city_id) %>%
    summarise(percent_complete = max(percent_complete)) %>%
    mutate(percent_category = percent_categoriser(percent_complete))
  ncap_ids <- location_presets %>%
    filter(name == "ncap_cities") %>%
    pull(location_id)
  in_ncap <- cities$city_id %in% ncap_ids

  groups <- list(
    c(id = "stations", label = "Monitoring stations (CAAQMS)"),
    c(id = "cities", label = "Cities"),
    c(id = "ncap", label = "NCAP cities"),
    c(id = "non_ncap", label = "Non-NCAP cities")
  )
  counts <- list(
    by_category(station_statuses$percent_category),
    by_category(cities$percent_category),
    by_category(cities$percent_category[in_ncap]),
    by_category(cities$percent_category[!in_ncap])
  )

  # from the data, not the API's live/inactive flag (see get_coverage_changes())
  changes <- if ("coverage_change" %in% names(station_statuses)) {
    names_with <- function(change) {
      I(sort(station_statuses$name[station_statuses$coverage_change == change]))
    }
    list(
      new = names_with("New"),
      reactivated = names_with("Reactivated"),
      removed = names_with("Removed"),
      new_cities = I(cities_with_new_data(station_statuses))
    )
  }

  list(
    groups = Map(function(g, n) c(as.list(g), n), groups, counts),
    changes = changes
  )
}

#' The site.json of one edition folder
#'
#' Every PNG in output/ is a card; a CSV is one only when its metadata marks it as a table.
snapshot_site_catalog <- function(period_dir) {
  stopifnot(dir.exists(period_dir))
  period <- snapshot_period(basename(normalizePath(period_dir)))
  output <- file.path(period_dir, "output")
  files <- list.files(output)
  read_meta <- function(stem) {
    path <- file.path(output, paste0(stem, ".json"))
    if (file.exists(path)) jsonlite::read_json(path) else NULL
  }
  card <- function(stem, kind, png, meta) {
    csv <- paste0(stem, ".csv")
    list(
      key = stem,
      section = snapshot_section_id(stem),
      kind = kind,
      files = list(all = list(
        png = if (!is.null(png)) file.path("output", png),
        csv = if (csv %in% files) file.path("output", csv),
        meta = meta
      ))
    )
  }

  pngs <- sort(files[grepl("\\.png$", files)])
  charts <- lapply(pngs, function(png) {
    stem <- sub("\\.png$", "", png)
    card(stem, "chart", png, read_meta(stem))
  })
  table_stems <- sub("\\.json$", "", files[grepl("\\.json$", files)])
  tables <- lapply(sort(table_stems), function(stem) {
    meta <- read_meta(stem)
    if (identical(meta$kind, "table")) card(stem, "table", NULL, meta)
  })

  inputs_file <- file.path(period_dir, "data_summary.csv")
  inputs <- if (file.exists(inputs_file)) {
    s <- utils::read.csv(inputs_file, stringsAsFactors = FALSE)
    lapply(seq_len(nrow(s)), function(i) as.list(s[i, ]))
  } else {
    list()
  }

  statuses_file <- file.path(output, "statuses.csv")
  presets_file <- file.path(period_dir, "cache", "location_presets.csv")
  coverage <- if (file.exists(statuses_file) && file.exists(presets_file)) {
    # the cache holds the API's raw IDs
    read <- function(file) normalise_city_ids(utils::read.csv(file, stringsAsFactors = FALSE))
    snapshot_coverage(
      station_statuses = read(statuses_file),
      location_presets = read(presets_file)
    )
  }

  list(
    focus_month = period$focus_month,
    edition_label = period$edition_label,
    generated = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    inputs = inputs,
    sections = c(
      lapply(snapshot_sections, function(s) s[c("id", "title", "blurb")]),
      list(list(id = "other", title = "Other outputs", blurb = ""))
    ),
    cards = c(charts, Filter(Negate(is.null), tables)),
    coverage = coverage
  )
}

#' Write <period>/site.json, and point <output_dir>/latest.json at it
build_snapshot_site <- function(output_dir, period, update_latest = TRUE) {
  catalog <- snapshot_site_catalog(file.path(output_dir, period))
  write_snapshot_json(catalog, file.path(output_dir, period, "site.json"))
  # half-year editions are extras, never the latest
  if (update_latest && is.null(catalog$edition_label)) {
    write_snapshot_json(
      list(month_dir = period, focus_month = catalog$focus_month),
      file.path(output_dir, "latest.json")
    )
  }
  log_info("Snapshot site: {length(catalog$cards)} cards in {period}/site.json")
  invisible(catalog)
}
