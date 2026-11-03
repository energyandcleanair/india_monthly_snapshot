parse_jsonish_column_value <- function(value) {
  if (is.na(value) || trimws(value) == "") {
    value <- "{}"
  }

  tryCatch(
    jsonlite::fromJSON(value),
    error = function(...) {
      jsonlite::fromJSON(convert_python_literal_to_json(value))
    }
  )
}

convert_python_literal_to_json <- function(value) {
  chars <- strsplit(value, "", fixed = TRUE)[[1]]
  output <- character()
  i <- 1

  while (i <= length(chars)) {
    char <- chars[[i]]

    if (char %in% c("'", "\"")) {
      string <- read_python_literal_string(chars, i)
      output <- c(output, jsonlite::toJSON(string$value, auto_unbox = TRUE))
      i <- string$next_index
    } else if (python_literal_token_at(chars, i, "None")) {
      output <- c(output, "null")
      i <- i + nchar("None")
    } else if (python_literal_token_at(chars, i, "True")) {
      output <- c(output, "true")
      i <- i + nchar("True")
    } else if (python_literal_token_at(chars, i, "False")) {
      output <- c(output, "false")
      i <- i + nchar("False")
    } else {
      output <- c(output, char)
      i <- i + 1
    }
  }

  paste0(output, collapse = "")
}

read_python_literal_string <- function(chars, start_index) {
  quote <- chars[[start_index]]
  value <- character()
  i <- start_index + 1

  while (i <= length(chars)) {
    char <- chars[[i]]

    if (char == "\\") {
      escaped <- read_python_literal_escape(chars, i)
      value <- c(value, escaped$value)
      i <- escaped$next_index
    } else if (char == quote) {
      return(list(
        value = paste0(value, collapse = ""),
        next_index = i + 1
      ))
    } else {
      value <- c(value, char)
      i <- i + 1
    }
  }

  stop("Unterminated string in Python literal")
}

read_python_literal_escape <- function(chars, start_index) {
  if (start_index == length(chars)) {
    return(list(value = "\\", next_index = start_index + 1))
  }

  escaped_char <- chars[[start_index + 1]]
  value <- switch(
    escaped_char,
    "n" = "\n",
    "r" = "\r",
    "t" = "\t",
    "b" = "\b",
    "f" = "\f",
    escaped_char
  )

  list(value = value, next_index = start_index + 2)
}

python_literal_token_at <- function(chars, start_index, token) {
  end_index <- start_index + nchar(token) - 1

  if (end_index > length(chars)) {
    return(FALSE)
  }

  candidate <- paste0(chars[start_index:end_index], collapse = "")
  if (candidate != token) {
    return(FALSE)
  }

  previous_char <- if (start_index == 1) "" else chars[[start_index - 1]]
  next_char <- if (end_index == length(chars)) "" else chars[[end_index + 1]]

  !is_python_identifier_char(previous_char) && !is_python_identifier_char(next_char)
}

is_python_identifier_char <- function(char) {
  grepl("^[A-Za-z0-9_]$", char)
}

unnest_json_columns <- function(df, column_name) {
  df %>%
    mutate(!!column_name := lapply(!!sym(column_name), parse_jsonish_column_value)) %>%
    tidyr::unnest_wider(!!sym(column_name))
}
