args <- commandArgs(trailingOnly = TRUE)
pkg_dir <- normalizePath(if (length(args) > 0L) args[[1L]] else getwd(), mustWork = TRUE)
lib <- tempfile("ibgb-installed-lib-")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)

install.packages(pkg_dir, lib = lib, repos = NULL, type = "source")
.libPaths(c(lib, .libPaths()))

library(iBiogeobears)

template <- system.file("templates", "analysis.yml", package = "iBiogeobears")
tree <- system.file("example_data", "tree.nwk", package = "iBiogeobears")
geography <- system.file("example_data", "geography.csv", package = "iBiogeobears")
regions <- system.file("example_data", "regions.csv", package = "iBiogeobears")

stopifnot(file.exists(template))
stopifnot(file.exists(tree))
stopifnot(file.exists(geography))
stopifnot(file.exists(regions))

example <- tempfile("ibgb-installed-example-")
project <- create_example_project(example)
result <- run_workflow(project$config, dry_run = TRUE, require_biogeobears = FALSE)

stopifnot(inherits(result, "iBGB_workflow_result"))
stopifnot(file.exists(file.path(result$project_paths$tables, "model_run_plan.csv")))
stopifnot(file.exists(file.path(result$project_paths$tables, "input_validation.csv")))
stopifnot(all(result$validation$ok))

cat("Installed package smoke test passed\n")
