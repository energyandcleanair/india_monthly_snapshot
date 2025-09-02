pkg_env_dirs <- new.env()

init_dirs <- function(
    ...,
    output_dir,
    subdir) {
  pkg_env_dirs$base <- output_dir
  pkg_env_dirs$month <- file.path(pkg_env_dirs$base, subdir)
  pkg_env_dirs$cache <- file.path(pkg_env_dirs$month, "cache")
  pkg_env_dirs$output <- file.path(pkg_env_dirs$month, "output")
  pkg_env_dirs$diag <- file.path(pkg_env_dirs$month, "diagnostics")

  for (dir_name in names(pkg_env_dirs)) {
    dir_path <- get(dir_name, envir = pkg_env_dirs)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
  }

  for (dir_name in names(pkg_env_dirs)) {
    dir_path <- get(dir_name, envir = pkg_env_dirs)
    log_debug(paste("Initialised directory:", dir_name, "with path:", dir_path))
  }

  lockEnvironment(pkg_env_dirs)
}

get_dir <- function(dir_name) {
  return(get(dir_name, envir = pkg_env_dirs))
}
