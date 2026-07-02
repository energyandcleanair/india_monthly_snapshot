write_measurement_completion_diagnostics <- function(...) {
  measurement_files <- list.files(
    get_dir("cache"),
    pattern = "^measurements_20[0-9]{2}-[0-9]{2}-[0-9]{2}_20[0-9]{2}-[0-9]{2}-[0-9]{2}[.]csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(measurement_files) == 0) {
    return(invisible(NULL))
  }

  read_measurement_month <- function(measurement_file) {
    measurements <- read.csv(measurement_file) %>%
      mutate(
        date = as.Date(date),
        month = lubridate::floor_date(date, "month")
      )

    month_start <- min(measurements$month)
    days_in_month <- lubridate::days_in_month(month_start)

    measurements %>%
      group_by(location_id, city_name, gadm1_name, gadm1_id) %>%
      summarise(
        days_with_data = n_distinct(date),
        .groups = "drop"
      ) %>%
      mutate(
        month = month_start,
        snapshot_month = format(month_start, "%Y-%m"),
        days_in_month = days_in_month,
        percent_days = days_with_data / days_in_month
      )
  }

  observed_completion <- lapply(
    measurement_files,
    read_measurement_month
  ) %>%
    bind_rows()

  location_metadata <- observed_completion %>%
    arrange(month) %>%
    group_by(location_id) %>%
    summarise(
      city_name = city_name[1],
      gadm1_name = gadm1_name[1],
      gadm1_id = gadm1_id[1],
      .groups = "drop"
    )

  month_metadata <- observed_completion %>%
    distinct(month, snapshot_month) %>%
    arrange(month) %>%
    mutate(days_in_month = lubridate::days_in_month(month))

  measurement_completion_monthly <- expand.grid(
    month = month_metadata$month,
    location_id = location_metadata$location_id,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    left_join(month_metadata, by = "month") %>%
    left_join(location_metadata, by = "location_id") %>%
    left_join(
      observed_completion %>%
        select(month, location_id, days_with_data, observed_percent_days = percent_days),
      by = c("month", "location_id")
    ) %>%
    mutate(
      days_with_data = ifelse(is.na(days_with_data), 0, days_with_data),
      percent_days = days_with_data / days_in_month
    ) %>%
    select(
      month,
      snapshot_month,
      location_id,
      city_name,
      gadm1_name,
      gadm1_id,
      days_with_data,
      days_in_month,
      percent_days
    ) %>%
    arrange(month, city_name, gadm1_name, location_id)

  snapshot_month_levels <- month_metadata$snapshot_month

  location_order <- measurement_completion_monthly %>%
    group_by(location_id) %>%
    summarise(
      mean_percent_days = mean(percent_days, na.rm = TRUE),
      city_name = city_name[1],
      gadm1_name = gadm1_name[1],
      .groups = "drop"
    ) %>%
    arrange(mean_percent_days, city_name, gadm1_name, location_id)
  location_levels <- location_order$location_id

  measurement_completion_monthly_summary <- measurement_completion_monthly %>%
    group_by(month, snapshot_month) %>%
    summarise(
      total_locations = n_distinct(location_id),
      locations_with_any_data = sum(days_with_data > 0),
      locations_with_80_percent_data = sum(percent_days >= 0.8),
      mean_percent_days = mean(percent_days, na.rm = TRUE),
      median_percent_days = median(percent_days, na.rm = TRUE),
      .groups = "drop"
    )

  period_under_80_locations <- location_order %>%
    filter(mean_percent_days <= 0.8) %>%
    arrange(mean_percent_days, city_name, gadm1_name, location_id) %>%
    mutate(location_label = paste0(city_name, ", ", gadm1_name))

  period_under_80_completion <- measurement_completion_monthly %>%
    filter(location_id %in% period_under_80_locations$location_id) %>%
    left_join(
      period_under_80_locations %>% select(location_id, location_label),
      by = "location_id"
    )

  period_under_80_location_order <- period_under_80_completion %>%
    group_by(location_id, location_label) %>%
    summarise(
      mean_percent_days = mean(percent_days, na.rm = TRUE),
      city_name = city_name[1],
      gadm1_name = gadm1_name[1],
      .groups = "drop"
    ) %>%
    arrange(mean_percent_days, city_name, gadm1_name, location_id)
  period_under_80_location_levels <- period_under_80_location_order$location_id
  period_under_80_location_labels <- setNames(
    period_under_80_location_order$location_label,
    period_under_80_location_order$location_id
  )

  write.csv(
    measurement_completion_monthly,
    file.path(get_dir("diag"), "measurement_completion_monthly.csv"),
    row.names = FALSE
  )

  write.csv(
    measurement_completion_monthly_summary,
    file.path(get_dir("diag"), "measurement_completion_monthly_summary.csv"),
    row.names = FALSE
  )

  write.csv(
    period_under_80_locations,
    file.path(get_dir("diag"), "measurement_completion_period_under_80.csv"),
    row.names = FALSE
  )

  heatmap_plot <- ggplot(
    measurement_completion_monthly,
    aes(
      x = factor(snapshot_month, levels = snapshot_month_levels),
      y = factor(location_id, levels = location_levels),
      fill = percent_days
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = "#F0F0F0",
      high = "#2B8CBE",
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      guide = ggplot2::guide_colorbar(barwidth = 8)
    ) +
    rcrea::theme_crea_new() +
    labs(
      title = "Monthly city PM2.5 data completion",
      subtitle = glue("{length(unique(measurement_completion_monthly$location_id))} locations across cached measurement files"),
      x = "",
      y = "",
      fill = "% days"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom"
    )

  ggplot2::ggsave(
    file.path(get_dir("diag"), "measurement_completion_monthly_heatmap.png"),
    plot = heatmap_plot,
    width = 12,
    height = 14,
    dpi = 300
  )

  if (nrow(period_under_80_locations) > 0) {
    period_under_80_heatmap_plot <- ggplot(
      period_under_80_completion,
      aes(
        x = factor(snapshot_month, levels = snapshot_month_levels),
        y = factor(location_id, levels = period_under_80_location_levels),
        fill = percent_days
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(
        low = "#F0F0F0",
        high = "#2B8CBE",
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1),
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        guide = ggplot2::guide_colorbar(barwidth = 8)
      ) +
      ggplot2::scale_y_discrete(labels = period_under_80_location_labels) +
      rcrea::theme_crea_new() +
      labs(
        title = "Locations with 80% or lower mean PM2.5 data completion",
        subtitle = glue("{nrow(period_under_80_locations)} locations; rows ordered by mean completion across cached months"),
        x = "",
        y = "",
        fill = "% days"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),
        legend.position = "bottom"
      )

    ggplot2::ggsave(
      file.path(get_dir("diag"), "measurement_completion_period_under_80_heatmap.png"),
      plot = period_under_80_heatmap_plot,
      width = 12,
      height = 16,
      dpi = 300
    )
  }

  monthly_plot <- ggplot(
    measurement_completion_monthly_summary,
    aes(x = factor(snapshot_month, levels = snapshot_month_levels), y = mean_percent_days)
  ) +
    geom_col(fill = "#2B8CBE") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    rcrea::theme_crea_new() +
    labs(
      title = "Mean city PM2.5 monthly completion",
      x = "",
      y = "Mean % days with data"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  ggplot2::ggsave(
    file.path(get_dir("diag"), "measurement_completion_monthly_summary.png"),
    plot = monthly_plot,
    width = 10,
    height = 6,
    dpi = 300
  )

  invisible(measurement_completion_monthly)
}
