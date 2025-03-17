
Warnings <- R6Class( # nolint: object_name_linter.
  "Warnings",
  public = list(
    data = NULL,
    initialize = function() {
      self$data <- tibble::tibble(
        type = character(),
        message = character()
      )
    },
    add_warning = function(type, message) {
      self$data <- dplyr::bind_rows(self$data, tibble::tibble(type = type, message = message))
    },
    add_warnings = function(warnings) {
      self$data <- dplyr::bind_rows(self$data, warnings %>% dplyr::select(type, message))
    },
    get_warnings = function() {
      return(self$data)
    }
  )
)
