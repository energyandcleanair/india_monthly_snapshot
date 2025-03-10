analysis <- function(){
  measurements_preset_ncap <- measurements %>%
    left_join(location_presets %>% filter(name == 'ncap_cities'),
              by = 'location_id',
              relationship = 'many-to-one') %>%
    mutate(name = replace_na(name, 'non_ncap_cities'),
           pass_who = value <= who_pm25_standard,
           pass_naaqs = value <= naaqs_pm25_standard,
           pass_naaqs2 = value <= 2 * naaqs_pm25_standard,
           grap_cat = cut(value, breaks = c(0, unlist(unname(scales_pm25))),
                          labels = names(scales_pm25)))

  measurements_preset_ncap_summary <- measurements_preset_ncap %>%
    group_by(location_id, city_name, pollutant, pollutant_name, name, gadm1_name) %>%
    summarise(mean = mean(value, na.rm = T)) %>%
    ungroup %>%
    mutate(pass_who = mean <= who_pm25_standard,
           pass_naaqs = mean <= naaqs_pm25_standard,
           pass_naaqs2 = mean <= 2 * naaqs_pm25_standard,
           grap_cat = cut(mean, breaks = c(0, unlist(unname(scales_pm25))),
                          labels = names(scales_pm25)))

  pass_count <- function(df){
    total <- nrow(df)
    pass_who <- nrow(df %>% filter(pass_who))
    not_pass_who <- nrow(df %>% filter(!pass_who))

    if(pass_who + not_pass_who != total){
      stop('pass_who + not_pass_who != total')
    }

    pass_naaqs <- nrow(df %>% filter(pass_naaqs))
    not_pass_naaqs <- nrow(df %>% filter(!pass_naaqs))

    if(pass_naaqs + not_pass_naaqs != total){
      stop('pass_naaqs + not_pass_naaqs != total')
    }

    pass_naaqs2 <- nrow(df %>% filter(pass_naaqs2))
    not_pass_naaqs2 <- nrow(df %>% filter(!pass_naaqs2))

    if(pass_naaqs2 + not_pass_naaqs2 != total){
      stop('pass_naaqs2 + not_pass_naaqs2 != total')
    }

    return(data.frame(total = total,
                      pass_who = pass_who,
                      not_pass_who = not_pass_who,
                      pass_naaqs = pass_naaqs,
                      not_pass_naaqs = not_pass_naaqs,
                      not_pass_naaqs2 = not_pass_naaqs2))
  }

  monthly_compliance <- lapply(measurements_preset_ncap_summary %>% distinct(name) %>% pull,
                               function(preset){
                                 pass_count(measurements_preset_ncap_summary %>% filter(name == preset)) %>%
                                   mutate(name = preset)
                               }) %>% bind_rows %>%
    select(name, everything())
  write.csv(monthly_compliance, file.path(get_dir('output'), 'monthly_compliance.csv'),
            row.names = F)


  daily_compliance <- measurements_preset_ncap %>% distinct(location_id, name) %>%
    apply(1, function(row){
      pass_count(measurements_preset_ncap %>%
                   filter(location_id == row['location_id'],
                          name == row['name'])) %>%
        mutate(location_id = row['location_id'],
               name = row['name'])
    }) %>% bind_rows %>%
    select(name, location_id, everything())


  get_compliance_frequency_breaks <- function(days_in_month){
    return(c(days_in_month * c(-0.01, 0.01, 0.25, 0.5, 0.75, 0.99, 1)))
  }

  day_freq_standard <- daily_compliance %>%
    mutate(pass_who_cut = cut(not_pass_who,
                              breaks = get_compliance_frequency_breaks(days_in_month),
                              labels = c('0%', '25%', '50%', '75%', '99%', '100%')),
           pass_naaqs_cut = cut(not_pass_naaqs,
                                breaks = get_compliance_frequency_breaks(days_in_month),
                                labels = c('0%', '25%', '50%', '75%', '99%', '100%')))

  day_freq_naaqs_summary <- day_freq_standard %>%
    group_by(name, pass_naaqs_cut) %>%
    summarise(value = n()) %>%
    ungroup() %>%
    right_join(expand.grid(name = unique(day_freq_standard$name),
                           pass_naaqs_cut = levels(day_freq_standard$pass_naaqs_cut)),
               by = c('name', 'pass_naaqs_cut')) %>%
    replace_na(list(value = 0))


  day_freq_naaqs_ncap_plot <- ggplot(day_freq_naaqs_summary %>% filter(name == 'ncap_cities'),
                                     aes(x = 2, y = value, fill = pass_naaqs_cut)) +
    geom_col(show.legend = F) +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = value),
              position = position_stack(vjust = 0.5)) +
    annotate("text", x = 0.25, y = 0,
             label = day_freq_naaqs_summary %>%
               filter(name == 'ncap_cities') %>%
               pull(value) %>%
               sum(),
             size = 6, fontface = "bold")

  day_freq_naaqs_nonncap_plot <- ggplot(day_freq_naaqs_summary %>% filter(name == 'non_ncap_cities'),
                                        aes(x = 2, y = value, fill = pass_naaqs_cut)) +
    geom_col(show.legend = F) +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = value),
              position = position_stack(vjust = 0.5)) +
    annotate("text", x = 0.25, y = 0,
             label = day_freq_naaqs_summary %>%
               filter(name == 'ncap_cities') %>%
               pull(value) %>%
               sum(),
             size = 6, fontface = "bold")

  day_freq_who_summary <- day_freq_standard %>% group_by(name, pass_who_cut) %>%
    summarise(value = n()) %>%
    ungroup() %>%
    right_join(expand.grid(name = unique(day_freq_standard$name),
                           pass_who_cut = levels(day_freq_standard$pass_who_cut)),
               by = c('name', 'pass_who_cut')) %>%
    replace_na(list(value = 0))

  day_freq_who_ncap_plot <- ggplot(day_freq_who_summary %>% filter(name == 'ncap_cities'),
                                   aes(x = 2, y = value, fill = pass_who_cut)) +
    geom_col(show.legend = F) +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = value),
              position = position_stack(vjust = 0.5)) +
    annotate("text", x = 0.25, y = 0,
             label = day_freq_who_summary %>%
               filter(name == 'ncap_cities') %>%
               pull(value) %>%
               sum(),
             size = 6, fontface = "bold")

  day_freq_who_nonncap_plot <- ggplot(day_freq_who_summary %>% filter(name == 'non_ncap_cities'),
                                      aes(x = 2, y = value, fill = pass_who_cut)) +
    geom_col() +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = value),
              position = position_stack(vjust = 0.5)) +
    labs(fill = '') +
    annotate("text", x = 0.25, y = 0,
             label = day_freq_who_summary %>%
               filter(name == 'ncap_cities') %>%
               pull(value) %>%
               sum(),
             size = 6, fontface = "bold")

  legend <- get_legend(day_freq_who_nonncap_plot)

  day_freq_who_nonncap_plot <- day_freq_who_nonncap_plot + theme(legend.position = 'none')

  cities_plot <- ggplot(monthly_compliance,
                        aes(x = 2, y = total, fill = name)) +
    geom_col(show.legend = F) +
    xlim(c(0.2, 2.5)) +
    theme_void() +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = total),
              position = position_stack(vjust = 0.5)) +
    labs(fill = '') +
    annotate("text", x = 0.25, y = 0,
             label = day_freq_naaqs_summary %>%
               pull(value) %>%
               sum(),
             size = 6, fontface = "bold")

  final_plot <- plot_grid(day_freq_naaqs_nonncap_plot, cities_plot, day_freq_naaqs_ncap_plot,
                          day_freq_who_nonncap_plot, legend, day_freq_who_ncap_plot,
                          ncol = 3)
  quicksave(file.path(get_dir('output'), 'compliance.png'), plot = final_plot)

  cities <- fetch_cities_for_india() %>% select(id, longitude, latitude)

  measurements_preset_ncap_summary <- measurements_preset_ncap_summary %>%
    left_join(cities %>% select(id, latitude, longitude), by = c('location_id' = 'id'))

  india_boundary <- sf::st_read('data/Indian_State_Boundary/India_State_Boundary_Updated.shp') %>%
    sf::st_make_valid()
  india_boundary_centroids <- sf::st_centroid(india_boundary)

  p <- ggplot() +
    layer_spatial(data = india_boundary, fill = 'white') +
    geom_text(data = india_boundary_centroids,
              aes(x = st_coordinates(geometry)[,1],
                  y = st_coordinates(geometry)[,2],
                  label = stname),
              size = 2) +
    layer_spatial(data = measurements_preset_ncap_summary %>%
                    filter(!is.na(latitude) & !is.na(longitude)) %>%
                    sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326),
                  aes(color = grap_cat),
                  size = 3) +
    scale_color_manual(values = grap_colors_pm25) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank())
  quicksave(file.path(get_dir('output'), 'cities_grap_distribution.png'),
            plot = p,
            scale = 1.5)

  measurements_preset_ncap_province <- measurements_preset_ncap_summary %>%
    group_by(gadm1_name, grap_cat) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = grap_cat, values_from = count, values_fill = list(count = 0)) %>%
    rename(`State/UT` = gadm1_name)
  write.csv(measurements_preset_ncap_province,
            file.path(get_dir('output'), 'cities_grap_distribution.csv'), row.names = F)


  measurements_grap <- measurements_preset_ncap %>%
    group_by(location_id, grap_cat) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = grap_cat, values_from = count, values_fill = list(count = 0)) %>%
    rowwise() %>%
    mutate(monitored_days = sum(across(any_of(c('Good', 'Satisfactory', 'Moderate',
                                                'Poor', 'Very Poor')))))


  monthly_cities_compliance <- lapply(measurements_preset_ncap %>% distinct(location_id) %>% pull,
                                      function(loc){
                                        pass_count(measurements_preset_ncap %>% filter(location_id == loc)) %>%
                                          mutate(location_id = loc)
                                      }) %>% bind_rows %>%
    mutate(`% days > NAAQS` = round(not_pass_naaqs / total * 100, 0)) %>%
    select(location_id, `% days > NAAQS`)


  measurements_top10_polluted_cities <- measurements_preset_ncap_summary %>%
    slice_max(n = 10, order_by = mean) %>%
    select(location_id, city_name, mean) %>%
    left_join(measurements_grap, by = 'location_id') %>%
    left_join(monthly_cities_compliance, by = 'location_id')
  write.csv(measurements_top10_polluted_cities,
            file.path(get_dir('output'), 'top10_polluted_cities.csv'), row.names = F)


  p <- ggplot(measurements_top10_polluted_cities, aes(x = factor(city_name, levels = city_name), y = mean, fill = mean)) +
    geom_col() +
    scale_fill_viridis_c() +
    theme_crea() +
    labs(title = glue('Top 10 most polluted cities in India by PM2.5 concentration - {month_year}',
                      month_year = format(focus_month, '%B %Y')),
         x = 'City',
         y = 'Mean PM2.5 concentration (µg/m³)') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 60, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 15, linetype = "dashed", color = "black") +
    geom_text(aes(x = 11, y = 60, label = "NAAQS"), color = "black", vjust = -0.5, hjust = 1.1) +
    geom_text(aes(x = 11, y = 15, label = "WHO"), color = "black", vjust = -0.5, hjust = 1.1)
  quicksave(file.path(get_dir('output'), 'top10_polluted_cities.png'), plot = p, scale = 1)
}
