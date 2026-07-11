test_that("ordinary-user guide is installed and reachable", {
  guide <- open_user_guide(browse = FALSE)
  expect_true(file.exists(guide))

  contents <- readLines(guide, warn = FALSE)
  required_text <- c(
    "check_installation()",
    "install_biogeobears(execute = TRUE)",
    "create_example_project",
    "run_workflow",
    "launch_app()",
    "bundle_diagnostics",
    "run_acceptance_check"
  )
  expect_true(all(vapply(required_text, function(x) any(grepl(x, contents, fixed = TRUE)), logical(1))))
})
