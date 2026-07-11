test_that("compare_node_state_sensitivity compares best non-J and +J nodes", {
  node_state_summary <- data.frame(
    model = rep(c("DEC", "DEC+J"), each = 2),
    location = "branch_top_at_node",
    node_index = rep(c(1L, 2L), times = 2),
    node_type = "tip",
    node_label = rep(c("sp1", "sp2"), times = 2),
    best_state = c("A", "B", "A", "AB"),
    best_probability = c(0.9, 0.7, 0.8, 0.6),
    stringsAsFactors = FALSE
  )
  comparison <- data.frame(
    model = c("DEC", "DEC+J"),
    has_j = c(FALSE, TRUE),
    AICc = c(10, 11)
  )

  out <- compare_node_state_sensitivity(node_state_summary, comparison)

  expect_equal(nrow(out), 2L)
  expect_true(all(c(
    "non_j_state",
    "plus_j_state",
    "state_differs",
    "probability_difference_abs"
  ) %in% names(out)))
  expect_false(out$state_differs[out$node_index == 1L])
  expect_true(out$state_differs[out$node_index == 2L])
})

test_that("compare_node_state_sensitivity returns stable empty table without paired models", {
  out <- compare_node_state_sensitivity(
    node_state_summary = data.frame(),
    comparison = data.frame(model = "DEC", has_j = FALSE, AICc = 1)
  )

  expect_equal(nrow(out), 0L)
  expect_true(all(c("non_j_model", "plus_j_model", "state_differs") %in% names(out)))
})

test_that("range-change event summaries classify ancestral state changes", {
  node_state_summary <- data.frame(
    model = "DEC",
    location = "branch_top_at_node",
    node_index = c(1L, 2L, 3L, 4L),
    node_type = c("tip", "tip", "internal", "internal"),
    node_label = c("sp1", "sp2", "node_3", "node_4"),
    best_state = c("A", "AB", "A", "null"),
    best_probability = c(0.9, 0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )
  tree_nodes <- data.frame(
    node_index = c(1L, 2L, 3L, 4L),
    node_type = c("tip", "tip", "internal", "internal"),
    node_label = c("sp1", "sp2", "node_3", "node_4"),
    parent_node_index = c(3L, 3L, 4L, NA_integer_),
    edge_length = c(1, 1, 1, NA_real_),
    stringsAsFactors = FALSE
  )
  geographic_states <- data.frame(
    state = c("null", "A", "B", "AB"),
    areas = c(NA, "A", "B", "A;B"),
    stringsAsFactors = FALSE
  )

  events <- summarize_range_change_events(node_state_summary, tree_nodes, geographic_states)
  summary <- summarize_range_change_event_counts(events)

  expect_true(all(c("event_type", "gained_areas", "lost_areas") %in% names(events)))
  expect_true("range_expansion" %in% events$event_type)
  expect_true("range_origin" %in% events$event_type)
  expect_equal(summary$event_count[match("Range expansion", summary$event_label)], 1L)
  expect_true(any(grepl("stochastic mapping", summary$interpretation_note, fixed = TRUE)))
})
