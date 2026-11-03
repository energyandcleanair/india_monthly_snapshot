summarise_top_polluted_city_frequency <- function(measurements_preset_ncap) {
  top_polluted_city_frequency <- measurements_preset_ncap %>%
    group_by(date, pollutant, pollutant_name) %>%
    slice_max(n = 10, order_by = value) %>%
    ungroup() %>%
    group_by(location_id, city_name, gadm1_name) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))

  top_polluted_city_frequency %>%
    add_city_display_labels() %>%
    arrange(desc(count), city_name, gadm1_name) %>%
    select(location_id, city_label, count) %>%
    ungroup()
}
