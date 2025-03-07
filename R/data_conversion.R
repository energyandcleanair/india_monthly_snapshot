unnest_json_columns <- function(df, column_name){
  df %>%
    mutate(!!column_name := gsub("'", "\"", !!sym(column_name))) %>%
    mutate(!!column_name := gsub("None", "null", !!sym(column_name))) %>%
    mutate(!!column_name := ifelse(is.na(!!sym(column_name)), "{}", !!sym(column_name))) %>%
    mutate(!!column_name := lapply(!!sym(column_name), jsonlite::fromJSON)) %>%
    tidyr::unnest_wider(!!sym(column_name))
}
