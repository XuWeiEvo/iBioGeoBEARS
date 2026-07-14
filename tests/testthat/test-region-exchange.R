make_exchange_bsm_tables <- function() {
  list(
    bsm_dispersal_routes = data.frame(
      model = "DEC",
      route_type = "all_dispersal",
      source_region = c("Region A", "Region B", "Region A"),
      target_region = c("Region B", "Region A", "Region A"),
      mean_count = c(2, 1, 0),
      stringsAsFactors = FALSE
    ),
    bsm_events = data.frame(
      model = "DEC",
      replicate = c(1L, 1L, 2L, 1L),
      event_type = c("d", "sympatry", "sympatry", "sympatry"),
      parent_state = c(NA, "A", "A", "B"),
      source_region_code = c("A", NA, NA, NA),
      target_region_code = c("B", NA, NA, NA),
      source_region = c("Region A", NA, NA, NA),
      target_region = c("Region B", NA, NA, NA),
      stringsAsFactors = FALSE
    ),
    bsm_event_summary = data.frame(
      model = "DEC", event_type = "total_events", replicate_count = 2L,
      stringsAsFactors = FALSE
    )
  )
}

test_that("summarize_region_exchange_matrix puts dispersal off-diagonal and in-situ on the diagonal", {
  long <- summarize_region_exchange_matrix(make_exchange_bsm_tables())

  expect_setequal(unique(long$kind), c("dispersal", "in_situ"))
  disp <- long[long$kind == "dispersal", , drop = FALSE]
  expect_equal(
    disp$mean_count[disp$source_region == "Region A" & disp$recipient_region == "Region B"], 2
  )
  # Self-route (A -> A, count 0) is not a dispersal exchange.
  expect_equal(nrow(disp[disp$source_region == disp$recipient_region, ]), 0L)

  ins <- long[long$kind == "in_situ", , drop = FALSE]
  # In-situ A: 2 events over 2 maps = 1.0; B: 1 event / 2 maps = 0.5; codes map to names.
  expect_equal(ins$mean_count[ins$source_region == "Region A"], 1.0)
  expect_equal(ins$mean_count[ins$source_region == "Region B"], 0.5)
})

test_that("format_region_exchange_matrix builds a wide matrix with totals", {
  wide <- format_region_exchange_matrix(summarize_region_exchange_matrix(make_exchange_bsm_tables()))

  expect_true(all(c("Source \\ Recipient", "Region A", "Region B", "Total (out)", "% out") %in% names(wide)))
  expect_true(any(wide[["Source \\ Recipient"]] == "Total (in)"))
  expect_true(any(wide[["Source \\ Recipient"]] == "% in"))
  # Diagonal holds in-situ (Region A row, Region A column) = 1.00.
  a_row <- wide[wide[["Source \\ Recipient"]] == "Region A", , drop = FALSE]
  expect_equal(a_row[["Region A"]], "1.00")

  expect_equal(nrow(format_region_exchange_matrix(data.frame())), 0L)
})
