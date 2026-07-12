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
    "create_windows_launcher()",
    "bundle_diagnostics",
    "run_acceptance_check"
  )
  expect_true(all(vapply(required_text, function(x) any(grepl(x, contents, fixed = TRUE)), logical(1))))
})

test_that("Chinese ordinary-user guide is installed and reachable", {
  guide <- open_user_guide(browse = FALSE, language = "zh-CN")
  expect_true(file.exists(guide))
  expect_match(basename(guide), "zh-CN", fixed = TRUE)

  contents <- readLines(guide, warn = FALSE, encoding = "UTF-8")
  required_text <- c(
    "中文快速开始",
    "check_installation()",
    "install_biogeobears(execute = TRUE)",
    "create_example_project",
    "run_workflow",
    "launch_app()",
    "create_windows_launcher()",
    "bundle_diagnostics"
  )
  expect_true(all(vapply(required_text, function(x) any(grepl(x, contents, fixed = TRUE)), logical(1))))
})
