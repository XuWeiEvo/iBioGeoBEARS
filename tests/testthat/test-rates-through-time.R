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
