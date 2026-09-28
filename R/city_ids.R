# The API re-keyed some cities at the end of June 2026 (old IDs have no data after
# 30 June), while the NCAP preset still lists three of them under their old IDs, and
# older years only exist under the old ones (2015-23 have both for some). Old IDs and
# names are mapped to the current ones, so a city is one city in every table.

city_id_aliases <- c(
  "bangalore_ind.16_1_in" = "bengaluru_ind.16_1_in",
  "asanol_ind.36_1_in" = "asansol_ind.36_1_in",
  "muradabad_ind.34_1_in" = "moradabad_ind.34_1_in",
  "gurgaon_ind.12_1_in" = "gurugram_ind.12_1_in",
  "manglore_ind.16_1_in" = "mangalore_ind.16_1_in",
  "chikkaballarpur_ind.16_1_in" = "chikkaballapur_ind.16_1_in",
  "greater_noida_ind.34_1_in" = "greater noida_ind.34_1_in"
)
city_name_aliases <- c(
  "Bangalore" = "Bengaluru",
  "Asanol" = "Asansol",
  "Muradabad" = "Moradabad",
  "Gurgaon" = "Gurugram",
  "Manglore" = "Mangalore",
  "Chikkaballarpur" = "Chikkaballapur",
  "Greater_Noida" = "Greater Noida"
)

#' Replace old city IDs (in `id_columns`) and names (in city_name) by the current ones
normalise_city_ids <- function(data, id_columns = c("city_id", "location_id")) {
  replace_alias <- function(x, aliases) {
    old <- !is.na(x) & x %in% names(aliases)
    x[old] <- unname(aliases[x[old]])
    x
  }
  for (column in intersect(id_columns, names(data))) {
    data[[column]] <- replace_alias(data[[column]], city_id_aliases)
  }
  if ("city_name" %in% names(data)) {
    data$city_name <- replace_alias(data$city_name, city_name_aliases)
  }
  data
}

#' City measurements with current IDs, one row per city and day
#'
#' Where the API has a day under both IDs, the current ID's value is kept.
normalise_city_measurements <- function(measurements) {
  measurements %>%
    mutate(old_id = location_id %in% names(city_id_aliases)) %>%
    normalise_city_ids() %>%
    arrange(old_id) %>%
    distinct(location_id, date, .keep_all = TRUE) %>%
    select(-old_id)
}
