test_that("run_models returns a dry-run plan", {
  cfg <- list(models = list(run = c("DEC", "DEC+J")))
  paths <- list(raw_biogeobears = tempfile("raw-bgb-"))

  plan <- run_models(cfg, paths, execute = FALSE)

  expect_equal(plan$model, c("DEC", "DEC+J"))
  expect_equal(plan$status, c("planned", "planned"))
  expect_true(all(grepl("DEC", plan$raw_output_dir)))
})

test_that("geography CSV is written in BioGeoBEARS data format", {
  geog_csv <- tempfile(fileext = ".csv")
  writeLines(c(
    "species,A,B,C",
    "sp1,1,0,0",
    "sp2,1,1,0",
    "sp3,0,0,1"
  ), geog_csv)

  geography <- read_range_matrix(geog_csv)
  geog_data <- tempfile(fileext = ".data")
  write_biogeobears_geography(geography, geog_data)

  lines <- readLines(geog_data, warn = FALSE)
  expect_equal(lines[1], "3\t3 (A B C)")
  expect_equal(lines[2], "sp1\t100")
  expect_equal(lines[3], "sp2\t110")
  expect_equal(lines[4], "sp3\t001")
})

test_that("run warnings are summarized for status tables", {
  summary <- summarize_run_warnings(c("optimizer warning", "optimizer warning", "", NA))

  expect_equal(summary$count, 1L)
  expect_equal(summary$messages, "optimizer warning")
})

test_that("state and node metadata tables have stable schemas", {
  states <- make_state_table(c("A", "B"), max_range_size = 2L, include_null_range = TRUE)
  expect_equal(states$state, c("null", "A", "B", "AB"))
  expect_equal(states$area_count, c(0L, 1L, 1L, 2L))
  expect_true(states$is_null_range[1L])

  nodes <- make_node_lookup(system.file("example_data", "tree.nwk", package = "iBiogeobears"))
  expect_true(all(c(
    "node_index",
    "node_type",
    "node_label",
    "parent_node_index",
    "edge_length",
    "is_root"
  ) %in% names(nodes)))
  expect_equal(sum(nodes$is_root), 1L)
})

test_that("run_models executes a DEC smoke run when BioGeoBEARS is available", {
  testthat::skip_if_not_installed("BioGeoBEARS")
  testthat::skip_if_not_installed("ape")

  out <- tempfile("ibgb-dec-smoke-")
  cfg <- list(
    project = list(name = "dec_smoke", output_dir = out),
    inputs = list(
      tree_file = system.file("example_data", "tree.nwk", package = "iBiogeobears"),
      geography_file = system.file("example_data", "geography.csv", package = "iBiogeobears"),
      regions_file = system.file("example_data", "regions.csv", package = "iBiogeobears"),
      max_range_size = 3L
    ),
    models = list(run = "DEC"),
    analysis = list(run_stochastic_mapping = FALSE),
    methodology = list(),
    advanced = list(),
    .config_file = system.file("templates", "analysis.yml", package = "iBiogeobears")
  )
  paths <- create_project(out)

  result <- suppressWarnings(run_models(cfg, paths, execute = TRUE))

  expect_equal(result$model, "DEC")
  expect_equal(result$status, "completed")
  expect_true(is.finite(result$logLik))
  expect_true(all(c("warning_count", "warning_messages") %in% names(attr(result, "run_status"))))
  expect_true(file.exists(file.path(paths$tables, "model_comparison.csv")))
  expect_true(file.exists(file.path(paths$tables, "geographic_states.csv")))
  expect_true(file.exists(file.path(paths$tables, "tree_nodes.csv")))
  expect_true(file.exists(file.path(paths$tables, "model_parameters.csv")))
  expect_true(file.exists(file.path(paths$tables, "ancestral_state_probabilities.csv")))
  expect_true(file.exists(file.path(paths$tables, "root_state_probabilities.csv")))
  expect_true(file.exists(file.path(paths$tables, "node_state_summary.csv")))

  run_status <- utils::read.csv(file.path(paths$tables, "model_run_status.csv"), check.names = FALSE)
  expect_true(all(c("warning_count", "warning_messages") %in% names(run_status)))

  node_summary <- utils::read.csv(file.path(paths$tables, "node_state_summary.csv"), check.names = FALSE)
  expect_true(all(c("model", "node_index", "best_state", "best_probability") %in% names(node_summary)))
})
