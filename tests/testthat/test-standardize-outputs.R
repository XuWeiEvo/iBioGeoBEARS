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
