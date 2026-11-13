## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  dpi = 110,
  out.width = "100%"
)
## the web font of the tables would be embedded in this page (8 MB); the website keeps it
options(lactater.web_font = identical(Sys.getenv("IN_PKGDOWN"), "true"))

## ----setup, message = FALSE---------------------------------------------------
library(lactater)
library(dplyr)

## ----tasks, echo = FALSE, eval = requireNamespace("gt", quietly = TRUE)-------
site <- "https://fmmattioni.github.io/lactater/articles/"
tasks <- tibble::tribble(
  ~step, ~task, ~functions, ~article,
  "Before the test", "Record every test the same way", "The web form, `read_recording()`, `recording_sheet()`",
  "best-practices|Best practices",
  "Analyze it", "Estimate every threshold", "`lactate_threshold()`", "methods|The methods and the evidence",
  "Analyze it", "See how the methods agree", "`autoplot()`", "",
  "Analyze it", "Check the data and the curve", "`lactate_checks()`, `lactate_diagnostics()`",
  "checking|Checking the analysis",
  "Analyze it", "Know how sure each threshold is", "`ci = TRUE`", "checking|Checking the analysis",
  "Use it", "Build three or five training zones", "`lactate_zones()`", "zones|Training zones",
  "Use it", "Count the training in each zone", "`training_load()`", "training-load|Training load",
  "Use it", "Predict race performance", "`predict_performance()`, `calibrate_performance()`",
  "performance|Predicting performance",
  "Use it", "Compare two tests, or follow a series", "`compare_tests()`, `curve_shape()`, `lactate_trend()`",
  "compare|Comparing tests",
  "Use it", "Compare with the population of the same age and sex", "`reference_percentiles()`",
  "compare|Comparing tests",
  "Share it", "Report the test to an athlete or a coach", "`test_report()`", "",
  "Share it", "A table for a paper", "`lactate_report()`", ""
)
parts <- strsplit(tasks$article, "|", fixed = TRUE)
tasks$article <- vapply(parts, function(p) if (length(p) == 2) sprintf("[%s](%s%s.html)", p[2], site, p[1]) else "", "")
gt::gt(tasks, groupname_col = "step") |>
  gt::fmt_markdown(columns = c("functions", "article")) |>
  gt::cols_label(task = "You want to", functions = "Use", article = "Read more") |>
  gt::cols_width(task ~ gt::pct(36), functions ~ gt::pct(38), article ~ gt::pct(26)) |>
  gt_lactater()

## -----------------------------------------------------------------------------
demo_data

## -----------------------------------------------------------------------------
results <- lactate_threshold(
  demo_data,
  intensity_column = intensity,
  lactate_column = lactate,
  heart_rate_column = heart_rate,
  rpe_column = rpe,
  plot = FALSE
)
results

## ----fig.height = 7.5, fig.width = 8------------------------------------------
autoplot(results)

## -----------------------------------------------------------------------------
autoplot(results, method = "ModDmax")

## -----------------------------------------------------------------------------
lactate_checks(results)
diagnostics <- lactate_diagnostics(results)
diagnostics

## ----fig.height = 11, fig.width = 9-------------------------------------------
autoplot(diagnostics)

## -----------------------------------------------------------------------------
set.seed(2024)
with_ci <- demo_data |>
  lactate_threshold(
    intensity, lactate, heart_rate,
    method = c("OBLA", "Dmax", "LTP"), plot = FALSE, ci = TRUE, n_boot = 200
  )

with_ci |>
  select(method, intensity, intensity_low, intensity_high)

## ----fig.height = 6.5---------------------------------------------------------
zones <- lactate_zones(results, model = 5)
zones
autoplot(zones)

## ----eval = requireNamespace("gt", quietly = TRUE)----------------------------
lactate_report(results)

## -----------------------------------------------------------------------------
lactate_tests |>
  filter(athlete == "cyclist_series") |>
  lactate_threshold(intensity, lactate, method = "Dmax", dmax_methods = "ModDmax", plot = FALSE, .by = "day")

