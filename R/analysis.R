measurements_preset_ncap <- measurements %>%
  left_join(location_presets %>% filter(name == 'ncap_cities'),
            by = 'location_id',
            relationship = 'many-to-one') %>%
  mutate(name = replace_na(name, 'non_ncap_cities'),
         pass_who = value <= who_pm25_standard,
         pass_naaqs = value <= naaqs_pm25_standard,
         pass_naaqs2 = value <= 2 * naaqs_pm25_standard,
         grap_cat = cut(mean, breaks = c(0, unlist(unname(scales_pm25))),
                        labels = names(scales_pm25)))

measurements_preset_ncap_summary <- measurements_preset_ncap %>%
  group_by(location_id, pollutant, pollutant_name, name, gadm1_name) %>%
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
quicksave('compliance.png', plot = final_plot)


stations_to_sf <- function(df){
  coordinates <- df$coordinates %>%
    sapply(function(x) gsub("'", "\"", x)) %>%
    # gsub("None", "null", infos)) %>%
    # mutate(infos = ifelse(is.na(infos), "{}", infos)) %>%
    lapply(function(x) jsonlite::fromJSON(x)) %>%
    lapply(function(x) c(unname(x[[2]]), unname(x[[1]]))) %>%
    do.call(rbind, .) %>%
    unname()

  df <- bind_cols(df, tibble(latitude = coordinates[,1], longitude = coordinates[,2]))

  df %>% sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)
}

stations <- fetch_current_stations_for_india() %>%
  stations_to_sf()

