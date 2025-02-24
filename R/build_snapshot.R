
#' @importFrom glue glue
#' @export
build_snapshot <- function(
    focus_month = NULL,
    output_dir = "output") {
  
  focus_month <- local({
    if (!is.null(focus_month)) {
      if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", focus_month)) {
        stop("focus_month must be in YYYY-MM-DD format")
      }
      return(
        lubridate::floor_date(
          lubridate::ymd(focus_month),
          "month"
        )
      )
    } else {
      today <- lubridate::today()
      start_of_this_month <- lubridate::floor_date(today, "month")
      start_of_last_month <- lubridate::floor_date(
        start_of_this_month - lubridate::days(1),
        "month"
      )

      return(start_of_last_month)
    }
  })

  init_dirs(format(focus_month, "%Y-%m"))

  focus_month_start <- lubridate::floor_date(focus_month, "month")
  focus_month_end <- lubridate::ceiling_date(focus_month, "month")

  # Get the data
  measurements <- fetch_measurements_for_india(
    start_date = focus_month_start,
    end_date = focus_month_end
  )

  # Check the data

  # Generate the charts and CSVs
}
