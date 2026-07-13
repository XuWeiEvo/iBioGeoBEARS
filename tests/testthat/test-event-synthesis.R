test_that("biogeographic_process_taxonomy has the expected structure", {
  taxonomy <- biogeographic_process_taxonomy()

  expect_equal(nrow(taxonomy), 7L)
  expect_setequal(
    names(taxonomy),
    c("process_key", "process_label", "process_group", "biogeobears_code", "bsm_event_type", "definition")
  )
  expect_true(all(c(
    "in_situ_speciation", "subset_sympatry", "vicariance",
    "founder_event_speciation", "range_expansion", "local_extinction",
    "range_switching"
  ) %in% taxonomy$process_key))
  expect_equal(sum(taxonomy$process_group == "cladogenetic"), 4L)
  expect_equal(sum(taxonomy$process_group == "anagenetic"), 3L)
  expect_false(any(duplicated(taxonomy$bsm_event_type)))
  expect_false(any(duplicated(taxonomy$process_key)))
})

test_that("summarize_biogeographic_processes maps codes and computes proportions", {
  bsm_event_summary <- data.frame(
    model = "DEC",
    event_type = c("sympatry", "vicariance", "founder", "d", "e", "a", "total_events"),
    mean_count = c(2, 1, 0, 3, 1, 0, 7),
    sd_count = c(0.5, 0.3, 0, 0.7, 0.2, 0, 1),
    sum_count = c(20, 10, 0, 30, 10, 0, 70),
    replicate_count = 10L,
    stringsAsFactors = FALSE
  )

  out <- summarize_biogeographic_processes(list(bsm_event_summary = bsm_event_summary))

  # The aggregate total_events row is not a process and is dropped.
  expect_equal(nrow(out), 6L)
  expect_equal(sum(out$process_group == "cladogenetic"), 3L)
  expect_equal(sum(out$process_group == "anagenetic"), 3L)

  sympatry <- out[out$biogeobears_code == "y", , drop = FALSE]
  expect_equal(sympatry$process_label, "In-situ (sympatric) speciation")
  expect_equal(sympatry$proportion_within_group, 2 / 3)
  expect_equal(sympatry$proportion_overall, 2 / 7)

  expansion <- out[out$biogeobears_code == "d", , drop = FALSE]
  expect_equal(expansion$process_label, "Range expansion")
  expect_equal(expansion$proportion_within_group, 3 / 4)
  expect_equal(expansion$proportion_overall, 3 / 7)

  # Cladogenetic processes are ordered before anagenetic ones per model.
  expect_equal(out$process_group[[1]], "cladogenetic")
  expect_equal(out$biogeobears_code[[1]], "y")
})

test_that("summarize_biogeographic_processes returns an empty table without BSM input", {
  empty_cols <- names(summarize_biogeographic_processes(list()))
  expect_equal(nrow(summarize_biogeographic_processes(list())), 0L)
  expect_true(all(c(
    "model", "process_group", "process_label", "mean_count",
    "proportion_within_group", "proportion_overall"
  ) %in% empty_cols))

  empty_summary <- data.frame(
    model = character(), event_type = character(), mean_count = numeric(),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(summarize_biogeographic_processes(list(bsm_event_summary = empty_summary))), 0L)
})

test_that("process synthesis reconciles with BioGeoBEARS class totals", {
  # Synthetic BioGeoBEARS count_ana_clado_events() summary_counts_BSMs structure:
  # rows are means/stdevs/sums, columns are event types including the aggregate
  # class totals all_clado, all_ana, and total_events that BioGeoBEARS reports.
  event_types <- c(
    "sympatry", "subset", "vicariance", "founder", "d", "e", "a",
    "all_clado", "all_ana", "ALL_disp", "ana_disp", "total_events"
  )
  means <- c(1.22, 1.84, 0.94, 0.50, 1.86, 0.30, 0.10, 4.50, 2.26, 2.26, 2.26, 6.76)
  summary_counts_BSMs <- as.data.frame(rbind(
    means = means,
    stdevs = rep(0.1, length(means)),
    sums = means * 50
  ))
  names(summary_counts_BSMs) <- event_types
  counts <- list(summary_counts_BSMs = summary_counts_BSMs)

  # Exercise the real standardization path used on BioGeoBEARS output.
  event_summary <- standardize_bsm_count_summary("DEC", counts, replicate_count = 50L)
  process <- summarize_biogeographic_processes(list(bsm_event_summary = event_summary))

  mean_of <- function(table, key, col = "mean_count") {
    table[[col]][table$event_type == key][[1]]
  }
  clado_sum <- sum(process$mean_count[process$process_group == "cladogenetic"])
  ana_sum <- sum(process$mean_count[process$process_group == "anagenetic"])

  # Per-process counts equal BioGeoBEARS' own event-type means, unchanged.
  expect_equal(process$mean_count[process$biogeobears_code == "j"], 0.50)
  expect_equal(process$mean_count[process$biogeobears_code == "d"], 1.86)

  # Per-process totals reconcile exactly with BioGeoBEARS class totals.
  expect_equal(clado_sum, mean_of(event_summary, "all_clado"))
  expect_equal(ana_sum, mean_of(event_summary, "all_ana"))
  expect_equal(clado_sum + ana_sum, mean_of(event_summary, "total_events"))
})

test_that("plot_biogeographic_process_synthesis returns a ggplot", {
  bsm_event_summary <- data.frame(
    model = c("DEC", "DEC", "DEC"),
    event_type = c("sympatry", "d", "e"),
    mean_count = c(2, 3, 1),
    sd_count = c(0.5, 0.7, 0.2),
    sum_count = c(20, 30, 10),
    replicate_count = 10L,
    stringsAsFactors = FALSE
  )
  process_summary <- summarize_biogeographic_processes(list(bsm_event_summary = bsm_event_summary))

  plot <- plot_biogeographic_process_synthesis(process_summary)
  expect_s3_class(plot, "ggplot")

  expect_error(
    plot_biogeographic_process_synthesis(data.frame(model = "DEC")),
    "missing required columns"
  )
})
