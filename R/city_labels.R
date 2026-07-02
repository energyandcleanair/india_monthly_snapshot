state_name_abbreviations <- c(
  "Andaman and Nicobar" = "AN",
  "Andaman and Nicobar Islands" = "AN",
  "Andhra Pradesh" = "AP",
  "Arunachal Pradesh" = "AR",
  "Assam" = "AS",
  "Bihar" = "BR",
  "Chandigarh" = "CH",
  "Chhattisgarh" = "CG",
  "Dadra and Nagar Haveli" = "DN",
  "Dadra and Nagar Haveli and Daman and Diu" = "DNHDD",
  "Daman and Diu" = "DD",
  "Delhi" = "DL",
  "Goa" = "GA",
  "Gujarat" = "GJ",
  "Haryana" = "HR",
  "Himachal Pradesh" = "HP",
  "Jammu and Kashmir" = "JK",
  "Jharkhand" = "JH",
  "Karnataka" = "KA",
  "Kerala" = "KL",
  "Ladakh" = "LA",
  "Lakshadweep" = "LD",
  "Madhya Pradesh" = "MP",
  "Maharashtra" = "MH",
  "Manipur" = "MN",
  "Meghalaya" = "ML",
  "Mizoram" = "MZ",
  "Nagaland" = "NL",
  "Odisha" = "OD",
  "Orissa" = "OD",
  "Puducherry" = "PY",
  "Punjab" = "PB",
  "Rajasthan" = "RJ",
  "Sikkim" = "SK",
  "Tamil Nadu" = "TN",
  "Telangana" = "TG",
  "Tripura" = "TR",
  "Uttar Pradesh" = "UP",
  "Uttarakhand" = "UK",
  "West Bengal" = "WB"
)

abbreviate_state_name <- function(state_name) {
  state_abbreviation <- state_name_abbreviations[state_name]
  ifelse(is.na(state_abbreviation), state_name, unname(state_abbreviation))
}

add_city_display_labels <- function(
    data,
    state_column = "gadm1_name",
    ncap_column = NULL) {
  city_label_states <- data %>%
    distinct(location_id, city_name, .data[[state_column]]) %>%
    group_by(city_name) %>%
    summarise(show_state = n_distinct(location_id) > 1, .groups = "drop")

  result <- data %>%
    left_join(city_label_states, by = "city_name") %>%
    mutate(
      state_abbreviation = abbreviate_state_name(.data[[state_column]]),
      city_label = case_when(
        show_state ~ paste0(city_name, ", ", state_abbreviation),
        TRUE ~ city_name
      )
    )

  if (!is.null(ncap_column)) {
    result <- result %>%
      mutate(
        city_label = case_when(
          .data[[ncap_column]] == "ncap_cities" ~ paste0(city_label, "*"),
          TRUE ~ city_label
        )
      )
  }

  result
}
