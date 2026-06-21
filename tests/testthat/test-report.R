test_that("render_report writes report source when quarto is unavailable", {
  out <- tempfile("ibgb-report-source-")
  paths <- create_project(out)
  result <- list(project_paths = paths)

  report <- render_report(result, format = "source")

  expect_true(file.exists(report))
  expect_match(basename(report), "summary_report[.]qmd")
  report_source <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(report_source, "Model Sensitivity Summary", fixed = TRUE)
  expect_match(report_source, "model_sensitivity.csv", fixed = TRUE)
})
