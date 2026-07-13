make_consistent_bsm_tables <- function() {
  event_summary <- data.frame(
    model = "DEC",
    event_type = c("all_clado", "all_ana", "total_events"),
    mean_count = c(4, 2, 6),
    replicate_count = 10L,
    stringsAsFactors = FALSE
  )
  process <- data.frame(
    model = "DEC",
    process_key = c("in_situ_speciation", "vicariance", "range_expansion"),
    process_group = c("cladogenetic", "cladogenetic", "anagenetic"),
    mean_count = c(2, 2, 2),
    stringsAsFactors = FALSE
  )
  budgets <- data.frame(
    model = "DEC", region = c("A", "B"),
    net_dispersal_flux = c(1, -1),
    stringsAsFactors = FALSE
  )
  rates <- data.frame(
    model = "DEC", process_key = "range_expansion",
    time_bin = c(1L, 2L), mean_count = c(1, 1),
    stringsAsFactors = FALSE
  )
  region_rates <- data.frame(
    model = "DEC", process_key = "range_expansion",
    region = c("A", "B"), time_bin = c(1L, 2L), mean_count = c(1, 1),
    stringsAsFactors = FALSE
  )
  run_status <- data.frame(
    model = "DEC", requested_maps = 10L, completed_maps = 10L,
    stringsAsFactors = FALSE
  )
  list(
    bsm_event_summary = event_summary,
    biogeographic_process_summary = process,
    region_process_budgets = budgets,
    process_rates_through_time = rates,
    region_process_rates_through_time = region_rates,
    bsm_run_status = run_status
  )
}

test_that("summarize_bsm_qc passes all checks on consistent tables", {
  qc <- summarize_bsm_qc(make_consistent_bsm_tables())

  expect_true(nrow(qc) >= 5L)
  expect_true(all(qc$status == "Pass"))
  expect_true(any(grepl("reconcile", qc$check)))
  expect_true(any(grepl("net dispersal flux", qc$check)))
  expect_true(any(grepl("sum across time bins", qc$check)))
  expect_true(any(grepl("across regions", qc$check)))
  expect_true(any(grepl("maps completed", qc$check)))
})

test_that("summarize_bsm_qc fails when class totals do not reconcile", {
  tables <- make_consistent_bsm_tables()
  tables$bsm_event_summary$mean_count[tables$bsm_event_summary$event_type == "all_clado"] <- 5
  qc <- summarize_bsm_qc(tables)
  recon <- qc[grepl("reconcile", qc$check), , drop = FALSE]
  expect_equal(recon$status, "Fail")
})

test_that("summarize_bsm_qc warns on incomplete stochastic maps", {
  tables <- make_consistent_bsm_tables()
  tables$bsm_run_status$completed_maps <- 6L
  qc <- summarize_bsm_qc(tables)
  maps <- qc[grepl("maps completed", qc$check), , drop = FALSE]
  expect_equal(maps$status, "Warning")
})

test_that("summarize_bsm_qc returns an empty table without BSM results", {
  expect_equal(nrow(summarize_bsm_qc(list())), 0L)
  expect_true(all(c("check", "model", "status", "detail") %in% names(summarize_bsm_qc(list()))))
})
