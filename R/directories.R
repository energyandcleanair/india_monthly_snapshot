pkg.env.dirs <- new.env()

init_dirs <- function(month_subdir = NULL) {
  pkg.env.dirs$base <- "data"
  pkg.env.dirs$month <- file.path(pkg.env.dirs$base, month_subdir)
  pkg.env.dirs$cache <- file.path(pkg.env.dirs$month, "cache")
  pkg.env.dirs$output <- file.path(pkg.env.dirs$month, "cache")
  pkg.env.dirs$diag <- file.path(pkg.env.dirs$month, "diagnostics")

  for (dir_name in names(pkg.env.dirs)) {
    dir_path <- get(dir_name, envir = pkg.env.dirs)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
  }

  lockEnvironment(pkg.env.dirs)
}

get_dir <- function(dir_name) {
  return(get(dir_name, envir = pkg.env.dirs))
}
