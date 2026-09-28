# The API renamed some cities' IDs but still uses the old ones in places: Bangalore became
# Bengaluru in July 2026, while the NCAP preset and the 2024-25 data still say Bangalore
# (and 2018-23 have both). Old IDs and names are mapped to the current ones, so a city is
# one city in every table.

city_id_aliases <- c("bangalore_ind.16_1_in" = "bengaluru_ind.16_1_in")
city_name_aliases <- c("Bangalore" = "Bengaluru")

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
