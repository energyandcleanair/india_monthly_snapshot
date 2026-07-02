---
name: run-r-tests
description: 'Run R package tests in this repository. Use when you need to execute the full testthat suite, rerun a specific test file, validate after code changes, or troubleshoot test failures in indiasnapshots.'
argument-hint: 'full suite | single test file | testthat direct run'
user-invocable: true
---

# Run R Tests

## When to Use

- Run the full test suite for this repository.
- Rerun a specific test file while debugging a failure.
- Validate package behavior after editing R code or tests.
- Check whether a failure is a real package failure or a test-runner artifact.

## Default Workflow

Use the package-aware runner from the repository root:

```sh
cd /workspaces/india_monthly_snapshot && Rscript -e "if (isFALSE(requireNamespace('pak', quietly = TRUE))) install.packages('pak', repos = 'https://cloud.r-project.org'); if (isFALSE(requireNamespace('devtools', quietly = TRUE))) pak::pkg_install('devtools'); devtools::test(reporter = 'summary')"
```

This is the preferred command for this repo because it loads the package source tree correctly.

## Why Not Start With test_dir

Some test files in this repository rely on package attachments or source-tree loading behavior that are present under `devtools::test()` but not always present under a direct `testthat::test_dir()` run.

If `test_dir()` reports failures such as missing `tibble()`, `tribble()`, or dplyr verbs, confirm with `devtools::test()` before treating them as package regressions.

## Run One Test File

Use this when you already know the failing file:

```sh
cd /workspaces/india_monthly_snapshot && Rscript -e "if (isFALSE(requireNamespace('pak', quietly = TRUE))) install.packages('pak', repos = 'https://cloud.r-project.org'); if (isFALSE(requireNamespace('devtools', quietly = TRUE))) pak::pkg_install('devtools'); devtools::test(filter = 'data_source', reporter = 'summary')"
```

Replace the filter string with a substring of the test file name, for example:

- `test_data_source.R` -> `filter = "data_source"`
- `test_data_conversion.R` -> `filter = "data_conversion"`
- `test_station_statuses.R` -> `filter = "station_statuses"`
- `test_warnings.R` -> `filter = "warnings"`

## Run Testthat Directly (Optional)

If you specifically need a direct testthat run (for example, to isolate a runner issue), use:

Run with:

```sh
cd /workspaces/india_monthly_snapshot && Rscript -e "if (isFALSE(requireNamespace('pak', quietly = TRUE))) install.packages('pak', repos = 'https://cloud.r-project.org'); if (isFALSE(requireNamespace('testthat', quietly = TRUE))) pak::pkg_install('testthat'); testthat::test_dir('tests/testthat', reporter = 'summary')"
```

Prefer `devtools::test()` as the default package-aware runner.

## Interpreting Results

- `Skipped` entries can be expected when tests contain explicit skip conditions.
- `Warnings` do not necessarily indicate test failure; review them separately from failed expectations.
- If direct `testthat::test_dir()` behavior differs from `devtools::test()`, treat `devtools::test()` as authoritative for package-level validation.

## Quick Checks After Environment Changes

After rebuilding the devcontainer or reinstalling dependencies, first verify the key native-library-sensitive packages still load:

```sh
cd /workspaces/india_monthly_snapshot && Rscript -e "pkgs <- c('creahelpers','rcrea','sf','terra','magick','units'); ok <- vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)); print(data.frame(package = pkgs, available = ok)); if (any(ok == FALSE)) quit(status = 1)"
```

Then run the full suite with `devtools::test()`. Always run the quick-check command via `Rscript` on the terminal, never paste it into an interactive R console, because `quit()` will close the session.
