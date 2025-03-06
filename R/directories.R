pkg.env.dirs <- new.env()

init_dirs <- function(
  ...,
  output_dir,
  month_subdir
) {
  pkg.env.dirs$base <- output_dir
  pkg.env.dirs$month <- file.path(pkg.env.dirs$base, month_subdir)
  pkg.env.dirs$cache <- file.path(pkg.env.dirs$month, "cache")
  pkg.env.dirs$output <- file.path(pkg.env.dirs$month, "output")
  pkg.env.dirs$diag <- file.path(pkg.env.dirs$month, "diagnostics")

  for (dir_name in names(pkg.env.dirs)) {
    dir_path <- get(dir_name, envir = pkg.env.dirs)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
  }

  for (dir_name in names(pkg.env.dirs)) {
    dir_path <- get(dir_name, envir = pkg.env.dirs)
    log_debug(paste("Initialised directory:", dir_name, "with path:", dir_path))
  }

  lockEnvironment(pkg.env.dirs)
}

get_dir <- function(dir_name) {
  return(get(dir_name, envir = pkg.env.dirs))
}
