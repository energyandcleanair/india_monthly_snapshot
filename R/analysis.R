analysis <- function(
    ...,
    cities,
    city_measurements,
    city_measurements_previous_years,
    station_measurements,
    location_presets,
    focus_month,
    days_in_month,
    warnings) {
  measurements <- city_measurements
  measurements_previous_years <- city_measurements_previous_years %>%
    mutate(year = lubridate::year(date), month = lubridate::month(date))
  focus_year <- focus_month %>% lubridate::year()

  measurements_preset_ncap <- measurements %>%
    left_join(
      location_presets %>% filter(name == "ncap_cities"),
      by = "location_id",
      relationship = "many-to-one"
    ) %>%
    mutate(
      name = replace_na(name, "non_ncap_cities"),
      pass_who = value <= who_pm25_standard,
      pass_naaqs = value <= naaqs_pm25_standard,
      pass_naaqs2 = value <= 2 * naaqs_pm25_standard,
      grap_cat = cut(
        value,
        breaks = c(0, unlist(unname(grap_scales_pm25))),
        labels = names(grap_scales_pm25)
      )
    )

  measurements_preset_ncap_summary <- measurements_preset_ncap %>%
    group_by(location_id, city_name, pollutant, pollutant_name, name, gadm1_name) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      pass_who = mean <= who_pm25_standard,
      pass_naaqs = mean <= naaqs_pm25_standard,
      pass_naaqs2 = mean <= 2 * naaqs_pm25_standard,
      grap_cat = cut(
        mean,
        breaks = c(0, unlist(unname(grap_scales_pm25))),
        labels = names(grap_scales_pm25)
      )
    )

  write.csv(
    measurements_preset_ncap_summary %>% arrange(desc(mean)),
    file.path(get_dir("output"), "all_cities_ordered.csv")
  )

  monthly_compliance <- lapply(
    measurements_preset_ncap_summary %>% distinct(name) %>% pull(),
    function(preset) {
      pass_count(
        measurements_preset_ncap_summary
        %>% filter(name == preset)
      ) %>%
        mutate(name = preset)
    }
  ) %>%
    bind_rows() %>%
    select(name, everything())
  write.csv(
    monthly_compliance, file.path(get_dir("output"), "monthly_compliance.csv"),
    row.names = FALSE
  )


  daily_compliance <- measurements_preset_ncap %>%
    distinct(location_id, name) %>%
    apply(1, function(row) {
      pass_count(
        measurements_preset_ncap %>%
          filter(
            location_id == row["location_id"],
            name == row["name"]
          )
      ) %>%
        mutate(
          location_id = row["location_id"],
          name = row["name"]
        )
    }) %>%
    bind_rows() %>%
    select(name, location_id, everything())

  day_freq_standard <- daily_compliance %>%
    mutate(
      pass_who_cut = cut(
        not_pass_who,
        breaks = get_compliance_frequency_breaks(days_in_month),
        labels = c("0%", "25%", "50%", "75%", "99%", "100%")
      ),
      pass_naaqs_cut = cut(
        not_pass_naaqs,
        breaks = get_compliance_frequency_breaks(days_in_month),
        labels = c("0%", "25%", "50%", "75%", "99%", "100%")
      )
    )

  day_freq_naaqs_summary <- day_freq_standard %>%
    group_by(name, pass_naaqs_cut) %>%
    summarise(value = n()) %>%
    ungroup() %>%
    right_join(
      expand.grid(
        name = unique(day_freq_standard$name),
        pass_naaqs_cut = levels(day_freq_standard$pass_naaqs_cut)
      ),
      by = c("name", "pass_naaqs_cut")
    ) %>%
    replace_na(list(value = 0))

  day_freq_who_summary <- day_freq_standard %>%
    group_by(name, pass_who_cut) %>%
    summarise(value = n()) %>%
    ungroup() %>%
    right_join(
      expand.grid(
        name = unique(day_freq_standard$name),
        pass_who_cut = levels(day_freq_standard$pass_who_cut)
      ),
      by = c("name", "pass_who_cut")
    ) %>%
    replace_na(list(value = 0))


  # NAAQS & WHO compliance plot ----
  ## NAAQS ----
  day_freq_naaqs_ncap_plot <- ggplot(
    day_freq_naaqs_summary %>% filter(name == "ncap_cities"),
    aes(x = 2, y = value, fill = pass_naaqs_cut)
  ) +
    geom_col(show.legend = FALSE) +
    xlim(c(1, 2.5)) +
    geom_text(
      aes(label = ifelse(value == 0, NA, value)),
      position = position_stack(vjust = 0.5),
      fontface = "bold",
      colour = "#ffffff"
    ) +
    labs(
      title = "Categorisation of NCAP cities \nagainst compliance to NAAQS guidelines"
    ) +
    annotate(
      "text",
      x = 1, y = 0,
      label = day_freq_naaqs_summary %>%
        filter(name == "ncap_cities") %>%
        pull(value) %>%
        sum(),
      size = 3,
      fontface = "bold"
    ) +
    rcrea::theme_crea_new() +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(
      values = c("#75b44c", "#27a59c", "#cc0000", "#990000", "#35416c", "#8cc9d0")
    ) +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

  day_freq_naaqs_nonncap_plot <- ggplot(
    day_freq_naaqs_summary %>% filter(name == "non_ncap_cities"),
    aes(x = 2, y = value, fill = pass_naaqs_cut)
  ) +
    geom_col(show.legend = FALSE) +
    xlim(c(1, 2.5)) +
    geom_text(
      aes(label = ifelse(value == 0, NA, value)),
      position = position_stack(vjust = 0.5),
      fontface = "bold",
      colour = "#ffffff"
    ) +
    labs(
      title = "Categorisation of non-NCAP cities \nagainst compliance to NAAQS guidelines"
    ) +
    annotate(
      "text",
      x = 1, y = 0,
      label = day_freq_naaqs_summary %>%
        filter(name == "non_ncap_cities") %>%
        pull(value) %>%
        sum(),
      size = 3,
      fontface = "bold"
    ) +
    rcrea::theme_crea_new() +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(
      values = c("#75b44c", "#27a59c", "#cc0000", "#990000", "#35416c", "#8cc9d0")
    ) +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

  ## WHO ----
  day_freq_who_ncap_plot <- ggplot(
    day_freq_who_summary %>% filter(name == "ncap_cities"),
    aes(x = 2, y = value, fill = pass_who_cut)
  ) +
    geom_col(show.legend = FALSE) +
    xlim(c(1, 2.5)) +
    geom_text(
      aes(label = ifelse(value == 0, NA, value)),
      position = position_stack(vjust = 0.5),
      fontface = "bold",
      colour = "#ffffff"
    ) +
    labs(
      title = "Categorisation of NCAP cities \nagainst compliance to WHO guidelines"
    ) +
    annotate(
      "text",
      x = 1, y = 0,
      label = day_freq_who_summary %>%
        filter(name == "ncap_cities") %>%
        pull(value) %>%
        sum(),
      size = 3,
      fontface = "bold"
    ) +
    rcrea::theme_crea_new() +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(
      values = c("#75b44c", "#27a59c", "#cc0000", "#990000", "#35416c", "#8cc9d0")
    ) +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

  day_freq_who_nonncap_plot <- ggplot(
    day_freq_who_summary %>% filter(name == "non_ncap_cities"),
    aes(x = 2, y = value, fill = pass_who_cut)
  ) +
    geom_col() +
    xlim(c(1, 2.5)) +
    geom_text(
      aes(label = ifelse(value == 0, NA, value)),
      position = position_stack(vjust = 0.5),
      fontface = "bold",
      colour = "#ffffff"
    ) +
    labs(
      fill = "% of days above standard",
      title = "Categorisation of non-NCAP cities \nagainst compliance to NAAQS guidelines"
    ) +
    annotate(
      "text",
      x = 1, y = 0,
      label = day_freq_who_summary %>%
        filter(name == "non_ncap_cities") %>%
        pull(value) %>%
        sum(),
      size = 3,
      fontface = "bold"
    ) +
    rcrea::theme_crea_new() +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(
      values = c("#75b44c", "#27a59c", "#cc0000", "#990000", "#35416c", "#8cc9d0"),
      labels = c("0%", "0-25%", "25-50%", "50-75%", "75-100%", "100%")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      legend.box.just = "center"
    )

  legend <- cowplot::get_legend(day_freq_who_nonncap_plot)

  day_freq_who_nonncap_plot <- day_freq_who_nonncap_plot + theme(legend.position = "none")

  ## Cities ----
  cities_plot <- ggplot(
    monthly_compliance,
    aes(x = 2, y = total, fill = name)
  ) +
    geom_col(show.legend = FALSE) +
    xlim(c(1, 2.5)) +
    geom_text(
      aes(label = glue("{ncap_grouping_labels[name]} \n{total}")),
      position = position_stack(vjust = 0.5),
      fontface = "bold",
      colour = "#ffffff",
      size = 2.2
    ) +
    labs(
      fill = "",
      title = "Total # of cities with >80% of the days \nwith CAAQMS data"
    ) +
    annotate(
      "text",
      x = 1, y = 0,
      label = day_freq_naaqs_summary %>%
        pull(value) %>%
        sum(),
      size = 3,
      fontface = "bold"
    ) +
    rcrea::theme_crea_new() +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("#35416c", "#8cc9D0")) +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

  final_plot <- plot_grid(
    day_freq_naaqs_nonncap_plot, cities_plot, day_freq_naaqs_ncap_plot,
    day_freq_who_nonncap_plot, legend, day_freq_who_ncap_plot,
    ncol = 3
  )
  rcrea::quicksave(file.path(get_dir("output"), "compliance.png"), plot = final_plot)

  measurements_preset_ncap_summary <- measurements_preset_ncap_summary %>%
    left_join(cities %>% select(id, latitude, longitude), by = c("location_id" = "id"))

  india_boundary <- sf::st_read(
    system.file(
      "extdata", "shp", "India_State_Boundary_Updated.shp",
      package = "indiasnapshots"
    )
  ) %>%
    sf::st_make_valid()

  sf::sf_use_s2(FALSE)
  india_boundary_centroids <- sf::st_centroid(india_boundary)
  sf::sf_use_s2(TRUE)


  # Cities GRAP distribution plot ----
  p <- ggplot() +
    ggspatial::layer_spatial(data = india_boundary, fill = "white") +
    geom_text(
      data = india_boundary_centroids,
      aes(
        x = sf::st_coordinates(geometry)[, 1],
        y = sf::st_coordinates(geometry)[, 2],
        label = stname
      ),
      size = 2
    ) +
    ggspatial::layer_spatial(
      data = measurements_preset_ncap_summary %>%
        filter(!is.na(latitude) & !is.na(longitude)) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
      aes(color = grap_cat),
      size = 3
    ) +
    scale_color_manual(values = grap_colors_pm25) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank()
    )
  rcrea::quicksave(
    file.path(get_dir("output"), "cities_grap_distribution.png"),
    plot = p,
    scale = 1
  )

  measurements_preset_ncap_province <- measurements_preset_ncap_summary %>%
    group_by(gadm1_name, grap_cat) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = grap_cat, values_from = count, values_fill = list(count = 0)) %>%
    rename(`State/UT` = gadm1_name)
  write.csv(
    measurements_preset_ncap_province,
    file.path(get_dir("output"), "cities_grap_distribution.csv"),
    row.names = FALSE
  )


  measurements_grap <- measurements_preset_ncap %>%
    group_by(location_id, grap_cat) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = grap_cat, values_from = count, values_fill = list(count = 0)) %>%
    rowwise() %>%
    mutate(monitored_days = sum(across(any_of(c(
      "Good", "Satisfactory", "Moderate", "Poor", "Very Poor"
    )))))


  monthly_cities_compliance <- lapply(
    measurements_preset_ncap %>% distinct(location_id) %>% pull(),
    function(loc) {
      pass_count(
        measurements_preset_ncap %>%
          filter(location_id == loc)
      ) %>%
        mutate(location_id = loc)
    }
  ) %>%
    bind_rows() %>%
    mutate(`% days > NAAQS` = round(not_pass_naaqs / total * 100, 0)) %>%
    select(location_id, `% days > NAAQS`)


  measurements_top10_polluted_cities <- measurements_preset_ncap_summary %>%
    slice_max(n = 10, order_by = mean) %>%
    select(location_id, city_name, gadm1_name, mean, name) %>%
    left_join(measurements_grap, by = "location_id") %>%
    left_join(monthly_cities_compliance, by = "location_id") %>%
    select(
      location_id, city_name, `State/UT` = gadm1_name, name, monitored_days, mean, `% days > NAAQS`,
      any_of(c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe"))
    )
  write.csv(
    measurements_top10_polluted_cities %>% select(-name),
    file.path(get_dir("output"), "top10_polluted_cities.csv"),
    row.names = FALSE
  )


  # Top 10 polluted cities plot ----
  p <- ggplot(
    measurements_top10_polluted_cities %>%
      mutate(
        city_name = case_when(
          name == "ncap_cities" ~ paste0(city_name, ",\n", `State/UT`, "*"),
          TRUE ~ paste0(city_name, ",\n", `State/UT`)
        )
      ),
    aes(x = factor(city_name, levels = city_name), y = mean, fill = factor(mean))
  ) +
    geom_col() +
    rcrea::scale_fill_crea_d() +
    rcrea::theme_crea_new() +
    labs(
      title = glue(
        "Top 10 most polluted cities in India by PM2.5 concentration - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      subtitle = "* indicates NCAP cities",
      x = "",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_text(aes(x = 11.25, y = 60, label = "NAAQS"), color = "black", vjust = -0.5, hjust = 1.1) +
    geom_text(aes(x = 11.25, y = 15, label = "WHO"), color = "black", vjust = -0.5, hjust = 1.1) +
    ggrepel::geom_text_repel(aes(label = round(mean, 0)), vjust = 1, size = 3.5)
  rcrea::quicksave(file.path(get_dir("output"), "top10_polluted_cities.png"), plot = p, scale = 1)


  measurements_top10_cleanest_cities <- measurements_preset_ncap_summary %>%
    slice_min(n = 10, order_by = mean) %>%
    select(location_id, city_name, gadm1_name, mean, name) %>%
    left_join(measurements_grap, by = "location_id") %>%
    left_join(monthly_cities_compliance, by = "location_id") %>%
    select(
      location_id, city_name, `State/UT` = gadm1_name, name, monitored_days, mean, `% days > NAAQS`,
      any_of(c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe"))
    )
  write.csv(
    measurements_top10_cleanest_cities %>% select(-name),
    file.path(get_dir("output"), "top10_cleanest_cities.csv"),
    row.names = FALSE
  )


  # Top 10 cleanest cities plot ----
  p <- ggplot(
    measurements_top10_cleanest_cities %>%
      mutate(
        city_name = case_when(
          name == "ncap_cities" ~ paste0(city_name, ",\n", `State/UT`, "*"),
          TRUE ~ paste0(city_name, ",\n", `State/UT`)
        )
      ),
    aes(x = factor(city_name, levels = city_name), y = mean, fill = factor(mean))
  ) +
    geom_col() +
    rcrea::scale_fill_crea_d() +
    rcrea::theme_crea_new() +
    labs(
      title = glue(
        "Top 10 most cleanest cities in India by PM2.5 concentration - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      subtitle = "* indicates NCAP cities",
      x = "",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_text(aes(x = 11.25, y = 60, label = "NAAQS"), color = "black", vjust = -0.5, hjust = 1.1) +
    geom_text(aes(x = 11.25, y = 15, label = "WHO"), color = "black", vjust = -0.5, hjust = 1.1) +
    ggrepel::geom_text_repel(aes(label = round(mean, 0)), vjust = 1, size = 3.5)
  rcrea::quicksave(file.path(get_dir("output"), "top10_cleanest_cities.png"), plot = p, scale = 1)


  cities_prev <- measurements_top10_polluted_cities %>%
    select(location_id) %>%
    pull()

  measurements_previous_years <- measurements_previous_years %>%
    left_join(
      location_presets %>% filter(name == "ncap_cities"),
      by = "location_id",
      relationship = "many-to-one"
    ) %>%
    mutate(
      name = replace_na(name, "non_ncap_cities"),
      pass_who = value <= who_pm25_standard,
      pass_naaqs = value <= naaqs_pm25_standard,
      pass_naaqs2 = value <= 2 * naaqs_pm25_standard,
      grap_cat = cut(
        value,
        breaks = c(0, unlist(unname(grap_scales_pm25))),
        labels = names(grap_scales_pm25)
      )
    )

  measurements_previous_years_summary <- measurements_previous_years %>%
    group_by(location_id, city_name, pollutant, pollutant_name, gadm1_name, name, month, year) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      pass_who = mean <= who_pm25_standard,
      pass_naaqs = mean <= naaqs_pm25_standard,
      pass_naaqs2 = mean <= 2 * naaqs_pm25_standard,
      grap_cat = cut(
        mean,
        breaks = c(0, unlist(unname(grap_scales_pm25))),
        labels = names(grap_scales_pm25)
      )
    )

  measurements_previous_years_grap <- measurements_previous_years %>%
    group_by(location_id, grap_cat, month, year) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = grap_cat, values_from = count, values_fill = list(count = 0)) %>%
    rowwise() %>%
    mutate(monitored_days = sum(across(any_of(c(
      "Good", "Satisfactory", "Moderate", "Poor", "Very Poor"
    )))))

  monthly_cities_compliance_previous_years <- lapply(
    measurements_previous_years %>% distinct(location_id) %>% pull(),
    function(loc) {
      lapply(
        measurements_previous_years %>% distinct(year) %>% pull(),
        function(yr) {
          pass_count(
            measurements_previous_years %>%
              filter(location_id == loc, year == yr)
          ) %>%
            mutate(year = yr)
        }
      ) %>%
        bind_rows() %>%
        mutate(location_id = loc)
    }
  ) %>%
    bind_rows() %>%
    mutate(`% days > NAAQS` = round(not_pass_naaqs / total * 100, 0)) %>%
    select(location_id, year, not_pass_naaqs, `% days > NAAQS`)

  measurements_10_polluted_cities_previous <- measurements_previous_years_summary %>%
    filter(year == focus_year - 1, location_id %in% cities_prev) %>%
    select(location_id, city_name, month, year, mean, gadm1_name, name) %>%
    left_join(
      measurements_previous_years_grap %>% filter(year == focus_year - 1),
      by = c("location_id", "month", "year")
    ) %>%
    left_join(
      monthly_cities_compliance_previous_years %>% filter(year == focus_year - 1),
      by = c("location_id", "year")
    ) %>%
    select(
      location_id, city_name, `State/UT` = gadm1_name, name,
      year, monitored_days, mean, `% days > NAAQS`,
      any_of(c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe"))
    )
  write.csv(
    measurements_10_polluted_cities_previous %>% select(-name),
    file.path(get_dir("output"), "top10_polluted_cities_prev.csv"),
    row.names = FALSE
  )


  # Top 10 polluted cities year-on-year plot ----
  p <- ggplot(
    bind_rows(
      measurements_top10_polluted_cities %>% mutate(year = focus_year),
      measurements_10_polluted_cities_previous
    ) %>%
      mutate(
        city_name = case_when(
          name == "ncap_cities" ~ paste0(city_name, ",\n", `State/UT`, "*"),
          TRUE ~ paste0(city_name, ",\n", `State/UT`)
        )
      ),
    aes(
      x = factor(city_name, levels = unique(city_name)),
      y = mean,
      fill = factor(year, levels = c(
        lubridate::year(focus_month),
        lubridate::year(focus_month) - 1
      ))
    )
  ) +
    geom_bar(position = "dodge", stat = "identity") +
    rcrea::scale_fill_crea_d() +
    rcrea::theme_crea_new() +
    labs(
      title = glue(
        "Year-on-year change of top 10 most polluted cities in India by PM2.5 concentration ",
        "- {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      subtitle = "* indicates NCAP cities",
      x = "",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_text(aes(x = 11.25, y = 60, label = "NAAQS"), color = "black", vjust = -0.5, hjust = 1.1) +
    geom_text(aes(x = 11.25, y = 15, label = "WHO"), color = "black", vjust = -0.5, hjust = 1.1) +
    geom_text(
      aes(label = round(mean, 0)),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3.5
    ) +
    theme(legend.title = element_blank())
  rcrea::quicksave(
    file.path(get_dir("output"), "top10_polluted_cities_year-on-year.png"),
    plot = p,
    scale = 1
  )


  measurements_preset_ncap_top10_count <- measurements_preset_ncap %>%
    group_by(date, pollutant, pollutant_name) %>%
    slice_max(n = 10, order_by = value) %>%
    group_by(location_id, city_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count))


  # Top 10 polluted cities frequency plot ----
  p <- ggplot(
    measurements_preset_ncap_top10_count,
    aes(x = factor(city_name, levels = city_name), y = count, fill = count)
  ) +
    geom_col() +
    rcrea::scale_fill_crea_c() +
    rcrea::theme_crea_new() +
    labs(
      title = glue(
        "Frequency of Indian cities in top 10 PM2.5 pollution rankings - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "none"
    ) +
    geom_text(aes(label = count), vjust = -0.5, size = 3)
  rcrea::quicksave(
    file.path(get_dir("output"), "top10_polluted_cities_freq.png"),
    plot = p,
    scale = 1
  )


  measurements_top_city_province <- measurements_preset_ncap_summary %>%
    group_by(`State/UT` = gadm1_name) %>%
    slice_max(n = 1, order_by = mean) %>%
    arrange(desc(mean)) %>%
    ungroup()

  # TODO maybe change to warning??
  actual_number_of_states <- length(
    measurements_top_city_province %>%
      distinct(gadm1_name) %>%
      pull()
  )
  expected_number_of_states <- 26

  if (actual_number_of_states < expected_number_of_states) {
    warnings$add_warning("wrong_state_number", paste(
      "Number of states in India in analysis was",
      actual_number_of_states,
      "less than",
      expected_number_of_states
    ))
  }


  # Top city by province plot ----
  p <- ggplot(
    measurements_top_city_province %>%
      mutate(
        city_name = case_when(
          name == "ncap_cities" ~ paste0(city_name, ",\n", `State/UT`, "*"),
          TRUE ~ paste0(city_name, ",\n", `State/UT`)
        )
      ),
    aes(
      x = factor(city_name, levels = city_name),
      y = mean,
      fill = mean
    )
  ) +
    geom_col() +
    rcrea::scale_fill_crea_c() +
    rcrea::theme_crea_new() +
    labs(
      title = glue(
        "Most polluted city in each state in India by PM2.5 concentration - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      subtitle = "* indicates NCAP cities",
      x = "",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_text(
      aes(x = nrow(measurements_top_city_province) + 2.25, y = 60, label = "NAAQS"),
      color = "black",
      vjust = -0.5,
      hjust = 1.1
    ) +
    geom_text(
      aes(x = nrow(measurements_top_city_province) + 2.25, y = 15, label = "WHO"),
      color = "black",
      vjust = -0.5,
      hjust = 1.1
    ) +
    geom_text(aes(label = round(mean, 0)), vjust = -0.5, size = 3)
  rcrea::quicksave(file.path(get_dir("output"), "top_city_province.png"), plot = p, scale = 1)


  measurements_capitals_summary <- measurements %>%
    left_join(
      location_presets %>% filter(name == "ncap_cities"),
      by = "location_id",
      relationship = "many-to-one"
    ) %>%
    filter(tolower(city_name) %in% tolower(states_capitals)) %>%
    group_by(location_id, city_name, pollutant, pollutant_name, `State/UT` = gadm1_name, name) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(mean))


  # State/provincial capital cities plot ----
  p <- ggplot(
    measurements_capitals_summary %>%
      mutate(
        city_name = case_when(
          name == "ncap_cities" ~ paste0(city_name, ",\n", `State/UT`, "*"),
          TRUE ~ paste0(city_name, ",\n", `State/UT`)
        )
      ),
    aes(
      x = factor(city_name, levels = city_name),
      y = mean,
      fill = mean
    )
  ) +
    geom_col() +
    rcrea::scale_fill_crea_c() +
    rcrea::theme_crea_new() +
    labs(
      title = glue(
        "PM2.5 concentrations across state/provincial capital cities in India - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      subtitle = "* indicates NCAP cities",
      x = "",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_text(
      aes(x = nrow(measurements_capitals_summary) + 2, y = 60, label = "NAAQS"),
      color = "black",
      vjust = -0.5,
      hjust = 1.1
    ) +
    geom_text(
      aes(x = nrow(measurements_capitals_summary) + 2, y = 15, label = "WHO"),
      color = "black",
      vjust = -0.5,
      hjust = 1.1
    ) +
    geom_text(aes(label = round(mean, 0)), vjust = -0.5, size = 3)
  rcrea::quicksave(file.path(get_dir("output"), "state_capitals.png"), plot = p, scale = 1)


  measurements_preset_igp_summary <- measurements %>%
    left_join(
      location_presets %>% filter(name == "igp_cities"),
      by = "location_id",
      relationship = "many-to-one"
    ) %>%
    mutate(name = replace_na(name, "non_igp_cities")) %>%
    filter(name == "igp_cities", city_name %in% igp_cities_million) %>%
    group_by(location_id, city_name, pollutant, pollutant_name, name, `State/UT` = gadm1_name) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(mean)) %>%
    left_join(
      cities %>% select(id, latitude, longitude),
      by = c("location_id" = "id")
    ) %>%
    mutate(grap_cat = cut(
      mean,
      breaks = c(0, unlist(unname(grap_scales_pm25))),
      labels = names(grap_scales_pm25)
    ))


  # IGP cities plot ----
  p <- ggplot(
    measurements_preset_igp_summary %>%
      mutate(
        city_name = case_when(
          name == "ncap_cities" ~ paste0(city_name, ",\n", `State/UT`, "*"),
          TRUE ~ paste0(city_name, ",\n", `State/UT`)
        )
      ),
    aes(
      x = factor(city_name, levels = city_name),
      y = mean,
      fill = mean
    )
  ) +
    geom_col() +
    rcrea::scale_fill_crea_c() +
    rcrea::theme_crea_new() +
    labs(
      title = glue(
        "PM2.5 concentrations across million plus cities in",
        " Indo-Gangetic Plain in India \n(with CAAQMS) - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "none"
    ) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_text(
      aes(x = nrow(measurements_preset_igp_summary) + 2, y = 60, label = "NAAQS"),
      color = "black",
      vjust = -0.5,
      hjust = 1.1
    ) +
    geom_text(
      aes(x = nrow(measurements_preset_igp_summary) + 2, y = 15, label = "WHO"),
      color = "black",
      vjust = -0.5,
      hjust = 1.1
    ) +
    geom_text(aes(label = round(mean, 0)), vjust = -0.5, size = 3)
  rcrea::quicksave(
    file.path(get_dir("output"), "igp_cities_million.png"),
    plot = p,
    scale = 1
  )


  # IGP cities GRAP distribution plot ----
  p <- ggplot() +
    ggspatial::layer_spatial(
      data = india_boundary %>% filter(tolower(stname) %in% tolower(igp_states)),
      fill = "white"
    ) +
    geom_text(
      data = india_boundary_centroids %>% filter(tolower(stname) %in% tolower(igp_states)),
      aes(
        x = sf::st_coordinates(geometry)[, 1],
        y = sf::st_coordinates(geometry)[, 2],
        label = stname
      ),
      size = 2
    ) +
    ggspatial::layer_spatial(
      data = measurements_preset_igp_summary %>%
        filter(!is.na(latitude) & !is.na(longitude)) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
      aes(color = grap_cat),
      size = 3
    ) +
    scale_color_manual(values = grap_colors_pm25) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank()
    )
  rcrea::quicksave(
    file.path(get_dir("output"), "igp_cities_grap_distribution.png"),
    plot = p,
    scale = 1
  )

  measurements_5_cities_summary <- measurements_preset_ncap_summary %>%
    filter(location_id %in% names(top5_populous_cities)) %>%
    select(location_id, city_name, mean) %>%
    left_join(measurements_grap, by = c("location_id")) %>%
    left_join(monthly_cities_compliance, by = c("location_id")) %>%
    mutate(
      month = lubridate::month(focus_month),
      year = focus_year
    )

  measurements_5_cities_summary_previous <- measurements_previous_years_summary %>%
    filter(location_id %in% names(top5_populous_cities)) %>%
    select(location_id, city_name, month, year, mean) %>%
    left_join(measurements_previous_years_grap, by = c("location_id", "month", "year")) %>%
    left_join(monthly_cities_compliance_previous_years, by = c("location_id", "year"))

  measurements_5_cities_summary_all <- bind_rows(
    measurements_5_cities_summary,
    measurements_5_cities_summary_previous
  )
  write.csv(
    measurements_5_cities_summary_all,
    file.path(get_dir("output"), "top5_populous_cities.csv"),
    row.names = FALSE
  )

  measurements_5_cities_all <- measurements %>%
    bind_rows(city_measurements_previous_years) %>%
    filter(location_id %in% names(top5_populous_cities))


  # Top 5 populous cities calendar plot ----
  sapply(top5_populous_cities, function(city) {
    n_years <- length(unique(lubridate::year(measurements_5_cities_all$date)))
    n_rows <- 2
    col_dim <- ceiling(n_years / n_rows)
    row_dim <- n_rows
    layout_dims <- c(col_dim, row_dim)

    plot_data <- measurements_5_cities_all %>%
      filter(city_name == city)
    plot_pm25(
      city_name = city,
      data = plot_data,
      value = "value",
      year_range = min(lubridate::year(plot_data$date)):max(lubridate::year(plot_data$date)),
      month_range = lubridate::month(focus_month),
      layout_dims = layout_dims,
      file_name = file.path(get_dir("output"), paste0("pm25_calendar_", city, ".png"))
    )
  })

  # add warning for cities with no coordinates
  measurements_preset_ncap_summary %>%
    filter(is.na(latitude) | is.na(longitude))
}


#' @importFrom dplyr filter
pass_count <- function(df) {
  total <- nrow(df)
  pass_who <- nrow(df %>% filter(pass_who))
  not_pass_who <- nrow(df %>% filter(!pass_who))

  if (pass_who + not_pass_who != total) {
    stop("pass_who + not_pass_who != total")
  }

  pass_naaqs <- nrow(df %>% filter(pass_naaqs))
  not_pass_naaqs <- nrow(df %>% filter(!pass_naaqs))

  if (pass_naaqs + not_pass_naaqs != total) {
    stop("pass_naaqs + not_pass_naaqs != total")
  }

  pass_naaqs2 <- nrow(df %>% filter(pass_naaqs2))
  not_pass_naaqs2 <- nrow(df %>% filter(!pass_naaqs2))

  if (pass_naaqs2 + not_pass_naaqs2 != total) {
    stop("pass_naaqs2 + not_pass_naaqs2 != total")
  }

  return(data.frame(
    total = total,
    pass_who = pass_who,
    not_pass_who = not_pass_who,
    pass_naaqs = pass_naaqs,
    not_pass_naaqs = not_pass_naaqs,
    not_pass_naaqs2 = not_pass_naaqs2
  ))
}


#' Calendar plot for PM2.5
#'
#' @param city_name
#' @param data
#' @param year_range
#' @param month_range
#' @param layout_dims
#' @param file_name
#'
#' @return export a png with file_name
#' @export
plot_pm25 <- function(
    ...,
    city_name,
    data,
    year_range,
    month_range,
    layout_dims,
    file_name,
    value) {
  plot <- openair::calendarPlot(
    data,
    pollutant = value,
    year = year_range,
    month = month_range,
    annotate = "date", # Limits don't work when set to date.
    breaks = c(0, 30, 60, 90, 120, 250, Inf),
    cols = c(
      "forestgreen", "light green", "yellow",
      "orange", "red", "dark red"
    ),
    labels = c("0-30", "31-60", "61-90", "91-120", "121-250", ">250"),
    w.shift = 2,
    layout = layout_dims, # Use the specified layout
    main = paste(city_name, "Daily PM2.5 Concentration (µg/m3)"),
    cex.date = 0.5,
    par.strip.text = list(cex = 0.50),
    par.settings = list(
      layout.heights = list(strip = 2),
      par.main.text = list(cex = 0.7), # Main title
      axis.text = list(cex = 0.5) # Axis tick labels
    )
  )
  # The font size inside the boxes can't be set directly when using openair::calendarPlot,
  # so we need to set the plot size to a large value.
  png(file_name, width = 1700, height = 900, res = 150)
  print(plot)
  dev.off()
}


get_compliance_frequency_breaks <- function(days_in_month) {
  return(c(days_in_month * c(-0.01, 0.01, 0.25, 0.5, 0.75, 0.99, 1)))
}
