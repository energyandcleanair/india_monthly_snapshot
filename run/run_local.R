# Expects to be run from the root of the project
# Usage: source("run/run_local.R") or Rscript run/run_local.R

devtools::install(".", quiet = TRUE, upgrade = "never")

indiasnapshots::build_snapshot(
  output_dir = "data"
)
