Warnings <- setRefClass(
  "Warnings",
  fields = list(
    data = "data.frame"
  ),
  methods = list(
    initialize = function() {
      data <<- tibble::tibble(
        type = character(),
        message = character()
      )
    },

    add_warning = function(type, message) {
      data <<- bind_rows(
        data, tibble::tibble(type = type, message = message)
      )
    },

    add_warnings = function(warnings) {
      data <<- bind_rows(
        data, warnings %>% select(type, message)
      )
    },

    get_warnings = function() {
      return(data)
    }
  )
)
