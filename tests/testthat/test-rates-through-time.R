make_rates_bsm_tables <- function() {
  events <- data.frame(
    model = "DEC",
    replicate = c(1L, 1L, 2L, 1L, 2L),
    event_type = c("sympatry", "sympatry", "sympatry", "d", "d"),
    event_time_before_present = c(8, 3, 9, 2, 7),
    stringsAsFactors = FALSE
  )
  event_summary <- data.frame(
    model = "DEC", event_type = "total_events", mean_count = 2.5, replicate_count = 2L,
    stringsAsFactors = FALSE
  )
  list(bsm_events = events, bsm_event_summary = event_summary)
}

test_that("summarize_process_rates_through_time bins events and computes mean/sd per map", {
  out <- summarize_process_rates_through_time(make_rates_bsm_tables(), n_bins = 2L)

  # 2 processes (in-situ speciation, range expansion) x 2 bins.
  expect_equal(nrow(out), 4L)
  expect_setequal(unique(out$process_group), c("cladogenetic", "anagenetic"))

  sympatry <- out[out$process_key == "in_situ_speciation", , drop = FALSE]
  sympatry <- sympatry[order(sympatry$time_bin), , drop = FALSE]
  # Bin 1 (recent): one map has the event, one does not -> mean 0.5.
  expect_equal(sympatry$mean_count[[1]], 0.5)
  expect_equal(sympatry$sd_count[[1]], stats::sd(c(1, 0)))
  # Bin 2 (old): both maps have one event -> mean 1, sd 0.
  expect_equal(sympatry$mean_count[[2]], 1)
  expect_equal(sympatry$sd_count[[2]], 0)

  # Conservation: mean counts across bins sum to total events / n_maps.
  expect_equal(sum(sympatry$mean_count), 3 / 2)
  expect_equal(sum(out$mean_count[out$process_key == "range_expansion"]), 2 / 2)

  # Bin geometry and rate.
  expect_equal(sympatry$bin_midpoint, c(2.25, 6.75))
  expect_equal(sympatry$rate[[2]], 1 / 4.5)

  # 95% CI from the 2.5/97.5 percentiles of the per-map counts brackets the mean.
  expect_true(all(c("ci_lower", "ci_upper") %in% names(out)))
  expect_true(all(out$ci_lower <= out$mean_count + 1e-9))
  expect_true(all(out$ci_upper >= out$mean_count - 1e-9))
  # Bin 1 in-situ: per-map counts c(1, 0) -> 2.5/97.5 percentiles are 0.025 and 0.975.
  expect_equal(sympatry$ci_lower[[1]], stats::quantile(c(1, 0), 0.025, names = FALSE))
  expect_equal(sympatry$ci_upper[[1]], stats::quantile(c(1, 0), 0.975, names = FALSE))
  # Bin 2 in-situ: both maps have exactly one -> CI collapses to 1.
  expect_equal(sympatry$ci_lower[[2]], 1)
  expect_equal(sympatry$ci_upper[[2]], 1)
})

test_that("summarize_process_rates_through_time returns an empty table without timed events", {
  empty_cols <- names(summarize_process_rates_through_time(list()))
  expect_equal(nrow(summarize_process_rates_through_time(list())), 0L)
  expect_true(all(c(
    "model", "process_label", "process_group", "bin_midpoint",
    "mean_count", "sd_count", "rate"
  ) %in% empty_cols))
})

test_that("plot_process_rates_through_time returns a ggplot", {
  rates <- summarize_process_rates_through_time(make_rates_bsm_tables(), n_bins = 3L)
  plot <- plot_process_rates_through_time(rates)
  expect_s3_class(plot, "ggplot")
  expect_error(
    plot_process_rates_through_time(data.frame(model = "DEC")),
    "missing required columns"
  )
})

make_region_rates_bsm_tables <- function() {
  events <- data.frame(
    model = "DEC",
    replicate = c(1L, 2L, 1L, 2L),
    event_type = c("d", "d", "sympatry", "sympatry"),
    event_time_before_present = c(2, 7, 3, 6),
    source_region_code = c("A", "B", NA, NA),
    target_region_code = c("B", "A", NA, NA),
    source_region = c("Region A", "Region B", NA, NA),
    target_region = c("Region B", "Region A", NA, NA),
    parent_state = c(NA, NA, "A", "B"),
    child_state = c("AB", "AB", "A,A", "B,B"),
    stringsAsFactors = FALSE
  )
  event_summary <- data.frame(
    model = "DEC", event_type = "total_events", mean_count = 2, replicate_count = 2L,
    stringsAsFactors = FALSE
  )
  list(bsm_events = events, bsm_event_summary = event_summary)
}

test_that("summarize_region_process_rates_through_time attributes in-situ, immigration and emigration", {
  out <- summarize_region_process_rates_through_time(make_region_rates_bsm_tables(), n_bins = 2L)

  expect_setequal(unique(out$process_label), c("Immigration", "Emigration", "In-situ speciation"))
  expect_setequal(unique(out$region), c("Region A", "Region B"))
  # 3 categories x 2 regions x 2 bins.
  expect_equal(nrow(out), 12L)

  # Dispersal A -> B in the recent bin: immigration into B, emigration out of A.
  imm_b <- out[out$process_key == "immigration" & out$region == "Region B", , drop = FALSE]
  imm_b <- imm_b[order(imm_b$time_bin), , drop = FALSE]
  expect_equal(imm_b$mean_count[[1]], 0.5)
  expect_equal(imm_b$mean_count[[2]], 0)

  emi_a <- out[out$process_key == "emigration" & out$region == "Region A", , drop = FALSE]
  emi_a <- emi_a[order(emi_a$time_bin), , drop = FALSE]
  expect_equal(emi_a$mean_count[[1]], 0.5)

  # In-situ speciation attributed to its single ancestral area (code -> name).
  ins_a <- out[out$process_key == "in_situ_speciation" & out$region == "Region A", , drop = FALSE]
  ins_a <- ins_a[order(ins_a$time_bin), , drop = FALSE]
  expect_equal(ins_a$mean_count[[1]], 0.5)
})

test_that("summarize_region_process_rates_through_time returns an empty table without region events", {
  empty_cols <- names(summarize_region_process_rates_through_time(list()))
  expect_equal(nrow(summarize_region_process_rates_through_time(list())), 0L)
  expect_true("region" %in% empty_cols)
})

test_that("plot_region_process_rates_through_time returns a ggplot and filters by process", {
  rates <- summarize_region_process_rates_through_time(make_region_rates_bsm_tables(), n_bins = 2L)
  expect_s3_class(plot_region_process_rates_through_time(rates), "ggplot")
  expect_s3_class(
    plot_region_process_rates_through_time(rates, process = "immigration"),
    "ggplot"
  )
  expect_error(
    plot_region_process_rates_through_time(data.frame(model = "DEC")),
    "missing required columns"
  )
})
