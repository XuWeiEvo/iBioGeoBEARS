test_that("check_installation returns actionable workflow readiness", {
  checks <- check_installation()

  expect_s3_class(checks, "data.frame")
  expect_true(all(c(
    "component", "required_for", "required", "status", "version", "next_step"
  ) %in% names(checks)))
  expect_true(all(c(
    "R", "Core R packages", "Shiny", "BioGeoBEARS", "Quarto HTML", "Quarto PDF"
  ) %in% checks$component))
  expect_true(all(checks$status %in% c("Ready", "Action needed")))
  expect_true(all(nzchar(checks$next_step)))
  expect_equal(checks$status[checks$component == "R"], "Ready")

  core <- checks[checks$component == "Core R packages", , drop = FALSE]
  if (identical(core$status[[1L]], "Ready")) {
    expect_equal(core$next_step[[1L]], "Ready.")
  } else {
    expect_match(core$next_step[[1L]], "Install missing packages", fixed = TRUE)
  }
})

test_that("check_installation can omit optional PDF readiness", {
  checks <- check_installation(include_pdf = FALSE)

  expect_false("Quarto PDF" %in% checks$component)
  expect_true("Quarto HTML" %in% checks$component)
})
