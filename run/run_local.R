# Expects to be run from the root of the project
# Usage: source("run/run_local.R") or Rscript run/run_local.R

devtools::load_all(".", quiet = TRUE)

india_monthly_snapshot::build_snapshot(
  focus_month = "2025-01",
  output_dir = "output",
  month_subdir = "2025-01"
)
