#' Summarize biogeographic process rates through time
#'
#' Bin the sampled BioGeoBEARS stochastic mapping (BSM) events by time before
#' present and summarize, per biogeographic process and time bin, the mean and
#' standard deviation of the event count per stochastic map, and the mean rate
#' (events per unit time). This gives a standardized view of when each
#' biogeographic process occurred that BioGeoBEARS does not produce directly.
#'
#' @param bsm_tables A list of standardized BSM tables containing at least
#'   `bsm_events`, and optionally `bsm_event_summary`, as returned in a workflow
#'   result's `bsm_tables`.
#' @param n_bins Number of equal-width time bins between the present and the
#'   oldest sampled event.
#' @return A data frame with one row per model, process, and time bin, including
#'   `bin_start`, `bin_end`, `bin_midpoint`, `mean_count`, `sd_count`, and
#'   `rate`. Returns an empty table with the same columns when no timed BSM
#'   events are available.
#' @export
summarize_process_rates_through_time <- function(bsm_tables, n_bins = 10L) {
  empty <- empty_process_rates_table()
  bsm_tables <- bsm_tables %||% list()
  events <- bsm_tables$bsm_events %||% NULL
  event_summary <- bsm_tables$bsm_event_summary %||% NULL
  if (is.null(events) || nrow(events) == 0L) {
    return(empty)
  }
  needed <- c("model", "replicate", "event_type", "event_time_before_present")
  if (!all(needed %in% names(events))) {
    return(empty)
  }
  n_bins <- max(1L, as.integer(n_bins))

  taxonomy <- biogeographic_process_taxonomy()
  ev <- events[
    events$event_type %in% taxonomy$bsm_event_type &
      !is.na(events$event_time_before_present),
    ,
    drop = FALSE
  ]
  if (nrow(ev) == 0L) {
    return(empty)
  }
  idx <- match(ev$event_type, taxonomy$bsm_event_type)
  ev$process_key <- taxonomy$process_key[idx]
  ev$process_label <- taxonomy$process_label[idx]
  ev$process_group <- taxonomy$process_group[idx]

  out <- do.call(rbind, lapply(split(ev, ev$model), function(model_ev) {
    process_rates_for_model(model_ev, event_summary, events, n_bins)
  }))
  if (is.null(out) || nrow(out) == 0L) {
    return(empty)
  }
  out <- out[order(out$model, out$process_group, out$process_label, out$time_bin), , drop = FALSE]
  row.names(out) <- NULL
  out[, names(empty), drop = FALSE]
}

process_rates_for_model <- function(model_ev, event_summary, events, n_bins) {
  model <- as.character(model_ev$model[[1L]])
  max_time <- max(model_ev$event_time_before_present, na.rm = TRUE)
  if (!is.finite(max_time) || max_time <= 0) {
    return(NULL)
  }
  breaks <- seq(0, max_time, length.out = n_bins + 1L)
  bin_width <- max_time / n_bins
  model_ev$bin <- findInterval(
    model_ev$event_time_before_present, breaks,
    rightmost.closed = TRUE, all.inside = TRUE
  )

  n_maps <- region_n_maps(event_summary, events, model)
  replicates <- sort(unique(model_ev$replicate))
  if (is.na(n_maps) || n_maps < length(replicates)) {
    n_maps <- length(replicates)
  }

  processes <- unique(model_ev[, c("process_key", "process_label", "process_group")])
  bins <- seq_len(n_bins)

  do.call(rbind, lapply(seq_len(nrow(processes)), function(i) {
    proc_rows <- model_ev[model_ev$process_key == processes$process_key[[i]], , drop = FALSE]
    do.call(rbind, lapply(bins, function(b) {
      in_bin <- proc_rows[proc_rows$bin == b, , drop = FALSE]
      per_map <- rep(0, n_maps)
      if (nrow(in_bin) > 0L) {
        map_counts <- table(factor(in_bin$replicate, levels = replicates))
        length(map_counts) <- n_maps
        map_counts[is.na(map_counts)] <- 0
        per_map <- as.numeric(map_counts)
      }
      mean_count <- mean(per_map)
      sd_count <- if (length(per_map) > 1L) stats::sd(per_map) else 0
      data.frame(
        model = model,
        process_key = processes$process_key[[i]],
        process_label = processes$process_label[[i]],
        process_group = processes$process_group[[i]],
        time_bin = b,
        bin_start = breaks[[b]],
        bin_end = breaks[[b + 1L]],
        bin_midpoint = (breaks[[b]] + breaks[[b + 1L]]) / 2,
        mean_count = mean_count,
        sd_count = sd_count,
        rate = if (bin_width > 0) mean_count / bin_width else NA_real_,
        interpretation_note = "Mean BioGeoBEARS stochastic mapping event count per map in the time bin.",
        stringsAsFactors = FALSE
      )
    }))
  }))
}

#' Plot biogeographic process rates through time
#'
#' Draw the mean event count per stochastic map through time for each
#' biogeographic process, with a standard-deviation ribbon, split into
#' cladogenetic and anagenetic process groups.
#'
#' @param process_rates A data frame from
#'   [summarize_process_rates_through_time()].
#' @return A ggplot object.
#' @export
plot_process_rates_through_time <- function(process_rates) {
  required <- c("model", "process_label", "process_group", "bin_midpoint", "mean_count")
  missing <- setdiff(required, names(process_rates))
  if (length(missing) > 0L) {
    stop("process_rates is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(process_rates) == 0L) {
    stop("process_rates must contain at least one row.", call. = FALSE)
  }
  process_rates$sd_count <- suppressWarnings(as.numeric(process_rates$sd_count %||% rep(NA_real_, nrow(process_rates))))
  group_labels <- c(cladogenetic = "Cladogenetic (speciation mode)", anagenetic = "Anagenetic (range evolution)")
  process_rates$group_label <- group_labels[process_rates$process_group]
  process_rates$group_label[is.na(process_rates$group_label)] <- process_rates$process_group[is.na(process_rates$group_label)]

  facets <- if (length(unique(process_rates$model)) > 1L) {
    ggplot2::facet_grid(stats::as.formula("group_label ~ model"))
  } else {
    ggplot2::facet_wrap(stats::as.formula("~ group_label"), ncol = 1L)
  }

  ggplot2::ggplot(process_rates, ggplot2::aes(x = bin_midpoint, y = mean_count, colour = process_label)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmax(0, mean_count - sd_count), ymax = mean_count + sd_count, fill = process_label),
      alpha = 0.12, colour = NA
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_x_reverse() +
    scale_colour_ibgb() +
    scale_fill_ibgb() +
    facets +
    ggplot2::labs(
      x = "Time before present",
      y = "Mean events per stochastic map",
      colour = NULL, fill = NULL,
      title = "Biogeographic process rates through time",
      subtitle = "Mean BSM event counts per time bin, with standard-deviation ribbons"
    ) +
    theme_ibgb() +
    ggplot2::guides(fill = "none")
}

empty_process_rates_table <- function() {
  data.frame(
    model = character(),
    process_key = character(),
    process_label = character(),
    process_group = character(),
    time_bin = integer(),
    bin_start = numeric(),
    bin_end = numeric(),
    bin_midpoint = numeric(),
    mean_count = numeric(),
    sd_count = numeric(),
    rate = numeric(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}
