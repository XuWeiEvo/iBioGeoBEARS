test_that("read_config fills defaults", {
  config <- system.file("templates", "analysis.yml", package = "BioGeoSyn")
  cfg <- read_config(config)
  expect_equal(cfg$project$name, "example_clade")
  expect_true("DEC+J" %in% cfg$models$run)
  expect_false(cfg$methodology$auto_declare_best_model)
  expect_false(cfg$analysis$run_stochastic_mapping)
  expect_equal(cfg$analysis$stochastic_mapping_model, "best")
  expect_equal(cfg$analysis$stochastic_mapping_replicates, 100L)
  expect_true(all(c(
    "times_file",
    "dists_file",
    "dispersal_multipliers_file",
    "areas_allowed_file",
    "areas_adjacency_file",
    "area_of_areas_file"
  ) %in% names(cfg$advanced$constraints)))
})

test_that("validate_inputs returns checks", {
  config <- system.file("templates", "analysis.yml", package = "BioGeoSyn")
  cfg <- read_config(config)
  checks <- validate_inputs(cfg)
  expect_true(all(c("check", "ok", "detail") %in% names(checks)))
  expect_true(all(c(
    "tree_geography_species_match",
    "geography_species_unique",
    "max_range_size_within_area_count",
    "models_not_duplicated",
    "output_parent_writable"
  ) %in% checks$check))
  expect_true(all(checks$ok))
  expect_true(all(c("label", "status", "next_step") %in% names(checks)))
  expect_true(all(checks$status == "Passed"))
  expect_true(all(checks$next_step == "No action needed."))
})

test_that("configuration defaults enable safe model resume", {
  cfg <- fill_config_defaults(list())

  expect_true(cfg$analysis$resume_completed_models)
  expect_false(cfg$analysis$retry_failed_only)
})

test_that("format_validation_results explains how to repair failures", {
  validation <- data.frame(
    check = c("tree_geography_species_match", "custom_future_check"),
    ok = c(FALSE, FALSE),
    detail = c("missing_from_geography: taxon_b", "custom detail"),
    stringsAsFactors = FALSE
  )

  formatted <- format_validation_results(validation)

  expect_equal(formatted$status, c("Needs attention", "Needs attention"))
  expect_equal(formatted$label[[1L]], "Tree and geography taxon names match")
  expect_match(formatted$next_step[[1L]], "identical", fixed = TRUE)
  expect_equal(formatted$label[[2L]], "Custom future check")
  expect_match(formatted$next_step[[2L]], "technical detail", fixed = TRUE)
  expect_equal(formatted$check, validation$check)
  expect_equal(formatted$detail, validation$detail)
})

test_that("validate_inputs catches common configuration errors", {
  config <- system.file("templates", "analysis.yml", package = "BioGeoSyn")
  cfg <- read_config(config)
  temp_dir <- tempfile("bgs-validation-")
  dir.create(temp_dir)

  bad_geography <- file.path(temp_dir, "bad_geography.csv")
  writeLines(c(
    "species,A,B,C",
    "sp1,1,0,0",
    "sp2,1,1,0",
    "spX,0,1,0",
    "sp4,0,1,1",
    "sp5,0,0,1"
  ), bad_geography)

  cfg$inputs$tree_file <- system.file("example_data", "tree.nwk", package = "BioGeoSyn")
  cfg$inputs$geography_file <- bad_geography
  cfg$inputs$regions_file <- system.file("example_data", "regions.csv", package = "BioGeoSyn")
  cfg$models$run <- c("DEC", "DEC")
  cfg$advanced$constraints <- list(dists_file = file.path(temp_dir, "missing_distances.txt"))

  checks <- validate_inputs(cfg)
  lookup <- stats::setNames(checks$ok, checks$check)

  expect_false(lookup[["tree_geography_species_match"]])
  expect_false(lookup[["models_not_duplicated"]])
  expect_false(lookup[["advanced_constraint_files_exist"]])
})

test_that("validate_inputs rejects a max range size narrower than the data", {
  # BioGeoBEARS aborts the whole run when any taxon is wider than
  # max_range_size, so this has to be caught before the models are fitted.
  cfg <- read_config(system.file("templates", "analysis.yml", package = "BioGeoSyn"))
  temp_dir <- tempfile("bgs-maxrange-")
  dir.create(temp_dir)

  geography <- file.path(temp_dir, "geography.csv")
  writeLines(c(
    "species,A,B,C,D",
    "sp1,1,0,0,0",
    "sp2,1,1,1,1",
    "sp3,1,1,1,0"
  ), geography)
  tree <- file.path(temp_dir, "tree.nwk")
  writeLines("((sp1:1,sp2:1):1,sp3:2);", tree)

  cfg$inputs$tree_file <- tree
  cfg$inputs$geography_file <- geography
  cfg$inputs$regions_file <- NULL
  cfg$advanced$constraints <- NULL

  cfg$inputs$max_range_size <- 3
  checks <- validate_inputs(cfg)
  row <- checks[checks$check == "max_range_size_covers_observed_ranges", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_false(row$ok)
  # The detail must name the offending taxon, not just report a count.
  expect_match(row$detail, "sp2", fixed = TRUE)
  expect_match(row$detail, "widest observed range=4", fixed = TRUE)
  expect_false(row$next_step == "No action needed.")
  # A too-narrow range is not the same failure as exceeding the area count.
  expect_true(checks$ok[checks$check == "max_range_size_within_area_count"])

  cfg$inputs$max_range_size <- 4
  relaxed <- validate_inputs(cfg)
  expect_true(relaxed$ok[relaxed$check == "max_range_size_covers_observed_ranges"])
})

test_that("every validation check has a catalogued label and next step", {
  cfg <- read_config(system.file("templates", "analysis.yml", package = "BioGeoSyn"))
  checks <- validate_inputs(cfg)
  catalog <- validation_check_catalog()

  expect_equal(nrow(catalog), length(unique(catalog$check)))
  expect_true(all(checks$check %in% catalog$check))
})
