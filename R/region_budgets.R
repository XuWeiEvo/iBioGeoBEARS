#' Summarize per-region biogeographic process budgets
#'
#' Aggregate BioGeoBEARS stochastic mapping (BSM) output into a per-region
#' budget of dispersal in and out and local extinction. Immigration and
#' emigration are mean dispersal counts into and out of each region (from all
#' dispersal routes); the net dispersal flux is immigration minus emigration, so
#' a positive value marks a net sink and a negative value a net source. Local
#' extinction is the mean number of range-contraction events lost in the region
#' per stochastic map.
#'
#' @param bsm_tables A list of standardized BSM tables containing at least
#'   `bsm_dispersal_routes`, and optionally `bsm_events` and `bsm_event_summary`,
#'   as returned in a workflow result's `bsm_tables`.
#' @return A data frame with one row per model and region, including
#'   `immigration`, `emigration`, `net_dispersal_flux`, `local_extinction`, and
#'   `total_dispersal`. Returns an empty table with the same columns when no BSM
#'   dispersal routes are available.
#' @export
summarize_region_process_budgets <- function(bsm_tables) {
  empty <- empty_region_process_budgets_table()
  bsm_tables <- bsm_tables %||% list()
  routes <- bsm_tables$bsm_dispersal_routes %||% NULL
  events <- bsm_tables$bsm_events %||% NULL
  event_summary <- bsm_tables$bsm_event_summary %||% NULL

  if (is.null(routes) || nrow(routes) == 0L) {
    return(empty)
  }
  required <- c("model", "route_type", "source_region", "target_region", "mean_count")
  if (!all(required %in% names(routes))) {
    return(empty)
  }
  disp <- routes[!is.na(routes$route_type) & routes$route_type == "all_dispersal", , drop = FALSE]
  disp <- disp[!is.na(disp$mean_count), , drop = FALSE]
  if (nrow(disp) == 0L) {
    return(empty)
  }

  out <- do.call(rbind, lapply(split(disp, disp$model), function(model_routes) {
    model <- as.character(model_routes$model[[1L]])
    regions <- sort(unique(c(model_routes$source_region, model_routes$target_region)))
    regions <- regions[!is.na(regions) & nzchar(regions)]
    if (length(regions) == 0L) {
      return(NULL)
    }
    emigration <- vapply(regions, function(r) {
      sum(model_routes$mean_count[model_routes$source_region == r], na.rm = TRUE)
    }, numeric(1))
    immigration <- vapply(regions, function(r) {
      sum(model_routes$mean_count[model_routes$target_region == r], na.rm = TRUE)
    }, numeric(1))
    local_extinction <- region_local_extinction(events, event_summary, model, regions)
    data.frame(
      model = model,
      region = regions,
      immigration = as.numeric(immigration),
      emigration = as.numeric(emigration),
      net_dispersal_flux = as.numeric(immigration - emigration),
      local_extinction = as.numeric(local_extinction),
      total_dispersal = as.numeric(immigration + emigration),
      interpretation_note = "Mean BioGeoBEARS stochastic mapping dispersal in/out and local extinction per region.",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }))
  if (is.null(out) || nrow(out) == 0L) {
    return(empty)
  }
  out <- out[order(out$model, -out$net_dispersal_flux, out$region), , drop = FALSE]
  row.names(out) <- NULL
  out[, names(empty), drop = FALSE]
}

region_local_extinction <- function(events, event_summary, model, regions) {
  out <- rep(0, length(regions))
  if (is.null(events) || nrow(events) == 0L || !"extirpation_region" %in% names(events)) {
    return(out)
  }
  ev <- events[
    !is.na(events$model) & events$model == model &
      !is.na(events$event_type) & events$event_type == "e",
    ,
    drop = FALSE
  ]
  if (nrow(ev) == 0L) {
    return(out)
  }
  n_maps <- region_n_maps(event_summary, events, model)
  if (is.na(n_maps) || n_maps <= 0L) {
    n_maps <- 1L
  }
  counts <- table(factor(ev$extirpation_region, levels = regions))
  as.numeric(counts) / n_maps
}

region_n_maps <- function(event_summary, events, model) {
  if (!is.null(event_summary) && all(c("replicate_count", "model") %in% names(event_summary))) {
    rc <- event_summary$replicate_count[event_summary$model == model]
    rc <- rc[!is.na(rc)]
    if (length(rc) > 0L) {
      return(max(rc))
    }
  }
  if (!is.null(events) && "replicate" %in% names(events)) {
    reps <- events$replicate[events$model == model]
    reps <- reps[!is.na(reps)]
    if (length(reps) > 0L) {
      return(length(unique(reps)))
    }
  }
  1L
}

#' Plot the regional dispersal budget
#'
#' Draw a diverging bar chart of the per-region dispersal budget: immigration
#' (positive) and emigration (negative) mean counts per region, with net
#' dispersal flux marked, so net sources and net sinks are read directly.
#'
#' @param region_budgets A data frame from [summarize_region_process_budgets()].
#' @return A ggplot object.
#' @export
plot_region_process_budget <- function(region_budgets) {
  required <- c("model", "region", "immigration", "emigration", "net_dispersal_flux")
  missing <- setdiff(required, names(region_budgets))
  if (length(missing) > 0L) {
    stop("region_budgets is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(region_budgets) == 0L) {
    stop("region_budgets must contain at least one row.", call. = FALSE)
  }

  region_order <- unique(region_budgets$region[order(region_budgets$net_dispersal_flux)])
  region_budgets$region <- factor(region_budgets$region, levels = region_order)

  long <- rbind(
    data.frame(
      model = region_budgets$model, region = region_budgets$region,
      direction = "Immigration (in)", count = region_budgets$immigration,
      stringsAsFactors = FALSE
    ),
    data.frame(
      model = region_budgets$model, region = region_budgets$region,
      direction = "Emigration (out)", count = -region_budgets$emigration,
      stringsAsFactors = FALSE
    )
  )

  ggplot2::ggplot(long, ggplot2::aes(x = region, y = count, fill = direction)) +
    ggplot2::geom_col(width = 0.7, colour = ibgb_palette()$outline, linewidth = 0.25) +
    ggplot2::geom_hline(yintercept = 0, colour = ibgb_palette()$muted, linewidth = 0.4) +
    ggplot2::geom_point(
      data = region_budgets,
      ggplot2::aes(x = region, y = net_dispersal_flux),
      inherit.aes = FALSE, size = 2, colour = ibgb_palette()$ink
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    ggplot2::scale_fill_manual(values = c(
      "Immigration (in)" = "#0072B2",
      "Emigration (out)" = "#D55E00"
    )) +
    ggplot2::labs(
      x = NULL,
      y = "Mean dispersal count per stochastic map (out < 0 < in)",
      fill = NULL,
      title = "Regional dispersal budget",
      subtitle = "Net dispersal flux (point) marks net sources (< 0) and net sinks (> 0)"
    ) +
    theme_ibgb() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

empty_region_process_budgets_table <- function() {
  data.frame(
    model = character(),
    region = character(),
    immigration = numeric(),
    emigration = numeric(),
    net_dispersal_flux = numeric(),
    local_extinction = numeric(),
    total_dispersal = numeric(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}
