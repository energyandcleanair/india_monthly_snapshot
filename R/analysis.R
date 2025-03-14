#' @importFrom tidyr replace_na pivot_wider
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom dplyr rowwise
#' @importFrom dplyr across
#' @importFrom dplyr slice_max
#' @importFrom dplyr arrange
#' @importFrom dplyr slice_min
#' @importFrom dplyr .data
#' @importFrom cowplot get_legend plot_grid
#' @importFrom ggspatial layer_spatial
#' @importFrom sf st_coordinates
analysis <- function(
    ...,
    city_measurements,
    city_measurements_previous_years,
    station_measurements,
    location_presets,
    focus_month,
    days_in_month,
    warnings) {
  measurements <- city_measurements
  focus_year <- focus_month %>% lubridate::year()

  measurements_preset_ncap <- measurements %>%
    left_join(location_presets %>% filter(name == "ncap_cities"),
      by = "location_id",
      relationship = "many-to-one"
    ) %>%
    mutate(
      name = replace_na(name, "non_ncap_cities"),
      pass_who = value <= who_pm25_standard,
      pass_naaqs = value <= naaqs_pm25_standard,
      pass_naaqs2 = value <= 2 * naaqs_pm25_standard,
      grap_cat = cut(value,
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
      grap_cat = cut(mean,
        breaks = c(0, unlist(unname(grap_scales_pm25))),
        labels = names(grap_scales_pm25)
      )
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
  write.csv(monthly_compliance, file.path(get_dir("output"), "monthly_compliance.csv"),
    row.names = FALSE
  )


  daily_compliance <- measurements_preset_ncap %>%
    distinct(location_id, name) %>%
    apply(1, function(row) {
      pass_count(measurements_preset_ncap %>%
        filter(
          location_id == row["location_id"],
          name == row["name"]
        )) %>%
        mutate(
          location_id = row["location_id"],
          name = row["name"]
        )
    }) %>%
    bind_rows() %>%
    select(name, location_id, everything())


  get_compliance_frequency_breaks <- function(days_in_month) {
    return(c(days_in_month * c(-0.01, 0.01, 0.25, 0.5, 0.75, 0.99, 1)))
  }

  day_freq_standard <- daily_compliance %>%
    mutate(
      pass_who_cut = cut(not_pass_who,
        breaks = get_compliance_frequency_breaks(days_in_month),
        labels = c("0%", "25%", "50%", "75%", "99%", "100%")
      ),
      pass_naaqs_cut = cut(not_pass_naaqs,
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


  day_freq_naaqs_ncap_plot <- ggplot(
    day_freq_naaqs_summary %>% filter(name == "ncap_cities"),
    aes(x = 2, y = value, fill = pass_naaqs_cut)
  ) +
    geom_col(show.legend = FALSE) +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    rcrea::scale_fill_crea_d() +
    geom_text(aes(label = value),
      position = position_stack(vjust = 0.5)
    ) +
    annotate("text",
      x = 0.25, y = 0,
      label = day_freq_naaqs_summary %>%
        filter(name == "ncap_cities") %>%
        pull(value) %>%
        sum(),
      size = 6, fontface = "bold"
    )

  day_freq_naaqs_nonncap_plot <- ggplot(
    day_freq_naaqs_summary %>% filter(name == "non_ncap_cities"),
    aes(x = 2, y = value, fill = pass_naaqs_cut)
  ) +
    geom_col(show.legend = FALSE) +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    rcrea::scale_fill_crea_d() +
    geom_text(aes(label = value),
      position = position_stack(vjust = 0.5)
    ) +
    annotate("text",
      x = 0.25, y = 0,
      label = day_freq_naaqs_summary %>%
        filter(name == "ncap_cities") %>%
        pull(value) %>%
        sum(),
      size = 6, fontface = "bold"
    )

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

  day_freq_who_ncap_plot <- ggplot(
    day_freq_who_summary %>% filter(name == "ncap_cities"),
    aes(x = 2, y = value, fill = pass_who_cut)
  ) +
    geom_col(show.legend = FALSE) +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    rcrea::scale_fill_crea_d() +
    geom_text(aes(label = value),
      position = position_stack(vjust = 0.5)
    ) +
    annotate("text",
      x = 0.25, y = 0,
      label = day_freq_who_summary %>%
        filter(name == "ncap_cities") %>%
        pull(value) %>%
        sum(),
      size = 6, fontface = "bold"
    )

  day_freq_who_nonncap_plot <- ggplot(
    day_freq_who_summary %>% filter(name == "non_ncap_cities"),
    aes(x = 2, y = value, fill = pass_who_cut)
  ) +
    geom_col() +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    rcrea::scale_fill_crea_d() +
    geom_text(aes(label = value),
      position = position_stack(vjust = 0.5)
    ) +
    labs(fill = "") +
    annotate("text",
      x = 0.25, y = 0,
      label = day_freq_who_summary %>%
        filter(name == "ncap_cities") %>%
        pull(value) %>%
        sum(),
      size = 6, fontface = "bold"
    )

  legend <- get_legend(day_freq_who_nonncap_plot)

  day_freq_who_nonncap_plot <- day_freq_who_nonncap_plot + theme(legend.position = "none")

  cities_plot <- ggplot(
    monthly_compliance,
    aes(x = 2, y = total, fill = name)
  ) +
    geom_col(show.legend = FALSE) +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    rcrea::scale_fill_crea_d() +
    geom_text(aes(label = total),
      position = position_stack(vjust = 0.5)
    ) +
    labs(fill = "") +
    annotate("text",
      x = 0.25, y = 0,
      label = day_freq_naaqs_summary %>%
        pull(value) %>%
        sum(),
      size = 6, fontface = "bold"
    )

  final_plot <- plot_grid(day_freq_naaqs_nonncap_plot, cities_plot, day_freq_naaqs_ncap_plot,
    day_freq_who_nonncap_plot, legend, day_freq_who_ncap_plot,
    ncol = 3
  )
  quicksave(file.path(get_dir("output"), "compliance.png"), plot = final_plot)

  cities <- fetch_cities_for_india() %>% select(id, longitude, latitude)

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

  p <- ggplot() +
    layer_spatial(data = india_boundary, fill = "white") +
    geom_text(
      data = india_boundary_centroids,
      aes(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2],
        label = stname
      ),
      size = 2
    ) +
    layer_spatial(
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
  quicksave(file.path(get_dir("output"), "cities_grap_distribution.png"),
    plot = p,
    scale = 1.5
  )

  measurements_preset_ncap_province <- measurements_preset_ncap_summary %>%
    group_by(gadm1_name, grap_cat) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = grap_cat, values_from = count, values_fill = list(count = 0)) %>%
    rename(`State/UT` = gadm1_name)
  write.csv(measurements_preset_ncap_province,
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
      "Good", "Satisfactory", "Moderate",
      "Poor", "Very Poor"
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
    select(location_id, city_name, gadm1_name, mean) %>%
    left_join(measurements_grap, by = "location_id") %>%
    left_join(monthly_cities_compliance, by = "location_id")
  write.csv(measurements_top10_polluted_cities,
    file.path(get_dir("output"), "top10_polluted_cities.csv"),
    row.names = FALSE
  )


  p <- ggplot(
    measurements_top10_polluted_cities,
    aes(x = factor(city_name, levels = city_name), y = mean, fill = mean)
  ) +
    geom_col() +
    scale_fill_viridis_c() +
    theme_crea() +
    labs(
      title = glue("Top 10 most polluted cities in India by PM2.5 concentration - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "City",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
    geom_text(aes(x = 11, y = 60, label = "NAAQS"), color = "black", vjust = -0.5, hjust = 1.1) +
    geom_text(aes(x = 11, y = 15, label = "WHO"), color = "black", vjust = -0.5, hjust = 1.1)
  quicksave(file.path(get_dir("output"), "top10_polluted_cities.png"), plot = p, scale = 1)


  measurements_top10_cleanest_cities <- measurements_preset_ncap_summary %>%
    slice_min(n = 10, order_by = mean) %>%
    select(location_id, city_name, mean) %>%
    left_join(measurements_grap, by = "location_id") %>%
    left_join(monthly_cities_compliance, by = "location_id")
  write.csv(measurements_top10_polluted_cities,
    file.path(get_dir("output"), "top10_cleanest_cities.csv"),
    row.names = FALSE
  )


  p <- ggplot(
    measurements_top10_cleanest_cities,
    aes(x = factor(city_name, levels = city_name), y = mean, fill = mean)
  ) +
    geom_col() +
    scale_fill_viridis_c() +
    theme_crea() +
    labs(
      title = glue("Top 10 most cleanest cities in India by PM2.5 concentration - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "City",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
    geom_text(aes(x = 11, y = 60, label = "NAAQS"), color = "black", vjust = -0.5, hjust = 1.1) +
    geom_text(aes(x = 11, y = 15, label = "WHO"), color = "black", vjust = -0.5, hjust = 1.1)
  quicksave(file.path(get_dir("output"), "top10_cleanest_cities.png"), plot = p, scale = 1)


  cities_prev <- measurements_top10_polluted_cities %>%
    select(location_id) %>%
    pull()

  measurements_prev <- city_measurements_previous_years %>%
    filter(city_id %in% cities_prev, lubridate::year(date) == focus_year - 1) %>%
    mutate(
      pass_who = value <= who_pm25_standard,
      pass_naaqs = value <= naaqs_pm25_standard,
      pass_naaqs2 = value <= 2 * naaqs_pm25_standard,
      grap_cat = cut(value,
        breaks = c(0, unlist(unname(grap_scales_pm25))),
        labels = names(grap_scales_pm25)
      )
    )

  measurements_prev_summary <- measurements_prev %>%
    group_by(location_id, city_name, pollutant, pollutant_name, gadm1_name) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      pass_who = mean <= who_pm25_standard,
      pass_naaqs = mean <= naaqs_pm25_standard,
      pass_naaqs2 = mean <= 2 * naaqs_pm25_standard,
      grap_cat = cut(mean,
        breaks = c(0, unlist(unname(grap_scales_pm25))),
        labels = names(grap_scales_pm25)
      )
    )

  measurements_prev_grap <- measurements_prev %>%
    group_by(location_id, grap_cat) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = grap_cat, values_from = count, values_fill = list(count = 0)) %>%
    rowwise() %>%
    mutate(monitored_days = sum(across(any_of(c(
      "Good", "Satisfactory", "Moderate",
      "Poor", "Very Poor"
    )))))

  monthly_cities_compliance_prev <- lapply(
    measurements_prev %>% distinct(location_id) %>% pull(),
    function(loc) {
      pass_count(
        measurements_prev %>%
          filter(location_id == loc)
      ) %>%
        mutate(location_id = loc)
    }
  ) %>%
    bind_rows() %>%
    mutate(`% days > NAAQS` = round(not_pass_naaqs / total * 100, 0)) %>%
    select(location_id, `% days > NAAQS`)

  measurements_top10_polluted_cities_prev <- measurements_prev_summary %>%
    select(location_id, city_name, mean) %>%
    left_join(measurements_prev_grap, by = "location_id") %>%
    left_join(monthly_cities_compliance_prev, by = "location_id")
  write.csv(measurements_top10_polluted_cities_prev,
    file.path(get_dir("output"), "top10_polluted_cities_prev.csv"),
    row.names = FALSE
  )

  p <- ggplot(
    bind_rows(
      measurements_top10_polluted_cities %>% mutate(year = year(focus_month)),
      measurements_top10_polluted_cities_prev %>% mutate(year = year(focus_month) - 1)
    ),
    aes(
      x = factor(city_name, levels = measurements_top10_polluted_cities %>% pull(city_name)),
      y = mean, , fill = factor(year, levels = c(year(focus_month), year(focus_month) - 1))
    )
  ) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_crea() +
    labs(
      title = glue("Top 10 most polluted cities in India by PM2.5 concentration - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "City",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
    geom_text(aes(x = 11, y = 60, label = "NAAQS"), color = "black", vjust = -0.5, hjust = 1.1) +
    geom_text(aes(x = 11, y = 15, label = "WHO"), color = "black", vjust = -0.5, hjust = 1.1) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank()
    )
  quicksave(file.path(get_dir("output"), "top10_polluted_cities_prev.png"), plot = p, scale = 1)


  measurements_preset_ncap_top10_count <- measurements_preset_ncap %>%
    group_by(date, pollutant, pollutant_name) %>%
    slice_max(n = 10, order_by = value) %>%
    group_by(location_id, city_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  p <- ggplot(measurements_preset_ncap_top10_count, aes(
    x = factor(city_name, levels = city_name),
    y = count,
    fill = count
  )) +
    geom_col() +
    scale_fill_viridis_c() +
    theme_crea() +
    labs(
      title = glue("Top 10 most polluted cities in India by PM2.5 concentration - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "City",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(label = count), vjust = -0.5, size = 3)
  quicksave(file.path(get_dir("output"), "top10_polluted_cities_freq.png"), plot = p, scale = 1)


  measurements_top_city_province <- measurements_preset_ncap_summary %>%
    group_by(gadm1_name) %>%
    slice_max(n = 1, order_by = mean) %>%
    arrange(desc(mean))

  # TODO maybe change to warning??
  actual_number_of_states <- length(
    measurements_top_city_province %>%
      distinct(gadm1_name) %>%
      pull()
  )
  expected_number_of_states <- 26
  if (actual_number_of_states != expected_number_of_states) {
    warnings$add_warning("wrong_state_number", paste(
      "Number of states in India in analysis was",
      actual_number_of_states,
      "not",
      expected_number_of_states
    ))
  }

  p <- ggplot(measurements_top_city_province, aes(
    x = factor(gadm1_name, levels = measurements_top_city_province %>% pull(gadm1_name)),
    y = mean,
    fill = mean
  )) +
    geom_col() +
    scale_fill_viridis_c() +
    theme_crea() +
    labs(
      title = glue(
        "Most polluted city in each state in India by PM2.5 concentration - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "State",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
    geom_text(
      aes(x = nrow(measurements_top_city_province) + 1, y = 60, label = "NAAQS"),
      color = "black",
      vjust = -0.5,
      hjust = 1.1
    ) +
    geom_text(
      aes(x = nrow(measurements_top_city_province) + 1, y = 15, label = "WHO"),
      color = "black",
      vjust = -0.5,
      hjust = 1.1
    ) +
    geom_text(aes(label = round(mean, 0)), vjust = -0.5, size = 3)
  quicksave(file.path(get_dir("output"), "top_city_province.png"), plot = p, scale = 1)


  measurements_capitals_summary <- measurements %>%
    filter(tolower(city_name) %in% tolower(states_capitals)) %>%
    group_by(location_id, city_name, pollutant, pollutant_name, gadm1_name) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(mean))

  p <- ggplot(measurements_capitals_summary, aes(
    x = factor(city_name, levels = measurements_capitals_summary %>% pull(city_name)),
    y = mean,
    fill = mean
  )) +
    geom_col() +
    scale_fill_viridis_c() +
    theme_crea() +
    labs(
      title = glue(
        "PM2.5 concentrations across state/provincial capital cities in India - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "City",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
    geom_text(
      aes(x = nrow(measurements_capitals_summary) + 1, y = 60, label = "NAAQS"),
      color = "black", vjust = -0.5, hjust = 1.1
    ) +
    geom_text(
      aes(x = nrow(measurements_capitals_summary) + 1, y = 15, label = "WHO"),
      color = "black", vjust = -0.5, hjust = 1.1
    ) +
    geom_text(aes(label = round(mean, 0)), vjust = -0.5, size = 3)
  quicksave(file.path(get_dir("output"), "state_capitals.png"), plot = p, scale = 1)


  measurements_preset_igp_summary <- measurements %>%
    left_join(location_presets %>% filter(name == "igp_cities"),
      by = "location_id",
      relationship = "many-to-one"
    ) %>%
    mutate(name = replace_na(name, "non_igp_cities")) %>%
    filter(name == "igp_cities", city_name %in% igp_cities_million) %>%
    group_by(location_id, city_name, pollutant, pollutant_name, name, gadm1_name) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(mean)) %>%
    left_join(
      cities %>%
        select(id, latitude, longitude),
      by = c("location_id" = "id")
    ) %>%
    mutate(grap_cat = cut(mean,
      breaks = c(0, unlist(unname(grap_scales_pm25))),
      labels = names(grap_scales_pm25)
    ))

  p <- ggplot(measurements_preset_igp_summary, aes(
    x = factor(city_name, levels = measurements_preset_igp_summary %>% pull(city_name)),
    y = mean,
    fill = mean
  )) +
    geom_col() +
    scale_fill_viridis_c() +
    theme_crea() +
    labs(
      title = glue(
        "PM2.5 concentrations across million plus cities in",
        " Indo-Gangetic Plain in India (with CAAQMS) - {month_year}",
        month_year = format(focus_month, "%B %Y")
      ),
      x = "City",
      y = "Mean PM2.5 concentration (µg/m³)"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
    geom_text(
      aes(x = nrow(measurements_preset_igp_summary) + 1, y = 60, label = "NAAQS"),
      color = "black", vjust = -0.5, hjust = 1.1
    ) +
    geom_text(
      aes(x = nrow(measurements_preset_igp_summary) + 1, y = 15, label = "WHO"),
      color = "black", vjust = -0.5, hjust = 1.1
    ) +
    geom_text(aes(label = round(mean, 0)), vjust = -0.5, size = 3)
  quicksave(file.path(get_dir("output"), "igp_cities_million.png"), plot = p, scale = 1)

  p <- ggplot() +
    layer_spatial(
      data = india_boundary %>% filter(tolower(stname) %in% tolower(igp_states)),
      fill = "white"
    ) +
    geom_text(
      data = india_boundary_centroids %>% filter(tolower(stname) %in% tolower(igp_states)),
      aes(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2],
        label = stname
      ),
      size = 2
    ) +
    layer_spatial(
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
  quicksave(file.path(get_dir("output"), "igp_cities_grap_distribution.png"),
    plot = p,
    scale = 1.5
  )
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
