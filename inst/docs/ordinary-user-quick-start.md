# iBiogeobears Ordinary-User Quick Start

This guide is for first-time users who want to run one single-clade
BioGeoBEARS analysis through `iBiogeobears`.

`iBiogeobears` does not bundle BioGeoBEARS. BioGeoBEARS must be installed
separately before real model execution. You can still run dry-run checks and
prepare projects before BioGeoBEARS is available.

## 1. Install iBiogeobears

Run this in R:

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/iBioGeoBEARS")
```

Load the package and check the local setup:

```r
library(iBiogeobears)
check_installation()
```

The `check_installation()` table tells you what is ready and what to install
next. HTML reports require Quarto. PDF reports also require a LaTeX engine.

## 2. Install BioGeoBEARS For Real Runs

Inspect the installation plan first:

```r
biogeobears_install_plan()
```

Then explicitly install BioGeoBEARS and its known helper packages:

```r
install_biogeobears(execute = TRUE)
```

Check again:

```r
check_installation()
check_biogeobears(required = FALSE)
```

## 3. Run The Built-In Example

Create a self-contained example project:

```r
project <- create_example_project("ibgb_example")
```

Run a dry check first. This does not require BioGeoBEARS:

```r
dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)
```

When BioGeoBEARS is installed, run the real six-model example:

```r
result <- run_workflow(project$config, dry_run = FALSE)
```

Render an HTML report and create shareable archives:

```r
report <- render_report(result, format = "html")
results_zip <- bundle_results(result)
diagnostics_zip <- bundle_diagnostics(result)
```

Start interpretation from these files:

```text
reports/summary_report.html
tables/shiny_run_summary.csv
tables/model_comparison.csv
tables/model_sensitivity.csv
tables/model_run_status.csv
```

## 4. Use The Shiny Runner

Install Shiny if needed:

```r
install.packages("shiny")
```

Launch the app:

```r
launch_app()
```

Recommended GUI flow:

1. Click `Create example project` or use `New project wizard`.
2. Click `Refresh setup checks`.
3. Keep `Dry run` checked and click `Run workflow`.
4. Fix any validation problems.
5. Uncheck `Dry run` only when BioGeoBEARS is installed.
6. Click `Run workflow`.
7. Click `Render report`.
8. Click `Create bundle if missing`.
9. Click `Create diagnostic bundle` if you need help debugging.

## 5. Use Your Own Data

The easiest path is the Shiny `New project wizard`.

Required files:

- Newick tree file.
- Geography CSV with one row per taxon and one column per area.
- Regions CSV describing area labels.

Always run a dry check before real BioGeoBEARS execution:

```r
project <- create_analysis_project(
  path = "my_ibgb_project",
  project_name = "my_clade",
  tree_file = "my_tree.nwk",
  geography_file = "my_geography.csv",
  regions_file = "my_regions.csv"
)

dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)
```

## 6. If Something Fails

First inspect:

```text
tables/input_validation.csv
tables/model_run_status.csv
logs/session_info.txt
```

If the workflow returned a `result` object, create a small diagnostics archive:

```r
diagnostics_zip <- bundle_diagnostics(result, overwrite = TRUE)
```

If the run stopped before returning `result`, send these files manually if they
exist:

```text
analysis.yml
results/<project>/tables/input_validation.csv
results/<project>/tables/model_run_status.csv
results/<project>/logs/session_info.txt
results/<project>/logs/
```

Common fixes:

- BioGeoBEARS missing: run `biogeobears_install_plan()` and then
  `install_biogeobears(execute = TRUE)`.
- Shiny missing: run `install.packages("shiny")`.
- HTML report missing: install Quarto, then run `check_report_environment()`.
- PDF report missing: install TinyTeX, MiKTeX, or TeX Live.
- Validation failed: open `tables/input_validation.csv` and follow the
  `repair_step` column.
- One model failed: open `tables/model_run_status.csv`, inspect the failed
  model log path, then rerun with `retry_failed_only = TRUE`.
- `+J` caution appears: this is not an execution error. It means the best or
  near-best statistical model includes founder-event jump dispersal and should
  be interpreted cautiously.

## 7. Stable-Release Readiness Check

Before sharing the package with ordinary users, maintainers should run:

```r
acceptance <- run_acceptance_check(
  "stable-release-acceptance",
  mode = "full"
)
acceptance
```

Full mode requires Shiny, BioGeoBEARS, and report rendering. It runs the
bundled example with all six supported models, checks model reuse, renders an
HTML report, and creates result and diagnostic bundles.
