make_region_bsm_tables <- function() {
  routes <- data.frame(
    model = "DEC",
    route_type = "all_dispersal",
    source_region = c("A", "A", "B", "B", "C", "C"),
    target_region = c("B", "C", "A", "C", "A", "B"),
    mean_count = c(1.0, 0.5, 0.2, 0.3, 0.1, 0.4),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    model = "DEC",
    replicate = c(1L, 2L, 1L),
    event_type = c("e", "e", "e"),
    extirpation_region = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )
  event_summary <- data.frame(
    model = "DEC", event_type = "total_events", mean_count = 5, replicate_count = 2L,
    stringsAsFactors = FALSE
  )
  list(bsm_dispersal_routes = routes, bsm_events = events, bsm_event_summary = event_summary)
}

test_that("summarize_region_process_budgets computes in/out flux and extinction", {
  out <- summarize_region_process_budgets(make_region_bsm_tables())

  expect_equal(nrow(out), 3L)
  a <- out[out$region == "A", , drop = FALSE]
  expect_equal(a$emigration, 1.5)
  expect_equal(a$immigration, 0.3)
  expect_equal(a$net_dispersal_flux, -1.2)
  expect_equal(a$total_dispersal, 1.8)
  # Two maps; region A lost in both -> mean 1.0, region B lost once -> 0.5.
  expect_equal(a$local_extinction, 1.0)
  expect_equal(out$local_extinction[out$region == "B"], 0.5)
  expect_equal(out$local_extinction[out$region == "C"], 0)

  # Ordered by descending net flux: B (net sink) first, A (net source) last.
  expect_equal(out$region[[1]], "B")
  expect_equal(out$region[[nrow(out)]], "A")

  # Invariant: every dispersal is one region's emigration and another's
  # immigration, so net flux sums to zero across regions.
  expect_equal(sum(out$net_dispersal_flux), 0)
})

test_that("summarize_region_process_budgets returns an empty table without routes", {
  empty_cols <- names(summarize_region_process_budgets(list()))
  expect_equal(nrow(summarize_region_process_budgets(list())), 0L)
  expect_true(all(c(
    "model", "region", "immigration", "emigration", "net_dispersal_flux",
    "local_extinction", "total_dispersal"
  ) %in% empty_cols))

  no_all <- data.frame(
    model = "DEC", route_type = "founder_event",
    source_region = "A", target_region = "B", mean_count = 1,
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(summarize_region_process_budgets(list(bsm_dispersal_routes = no_all))), 0L)
})

test_that("plot_region_process_budget returns a ggplot", {
  budgets <- summarize_region_process_budgets(make_region_bsm_tables())
  plot <- plot_region_process_budget(budgets)
  expect_s3_class(plot, "ggplot")
  expect_error(
    plot_region_process_budget(data.frame(model = "DEC")),
    "missing required columns"
  )
})
