standardize_biogeobears_outputs <- function(model_results, prepared_inputs, project_paths) {
  completed <- model_results[
    vapply(model_results, function(x) identical(x$status, "completed") && !is.null(x$result), logical(1))
  ]

  empty_outputs <- list(
    geographic_states = data.frame(),
    tree_nodes = data.frame(),
    parameter_table = data.frame(),
    ancestral_state_probabilities = data.frame(),
    root_state_probabilities = data.frame(),
    node_state_summary = data.frame()
  )
  if (length(completed) == 0L) {
    return(empty_outputs)
  }

  parameter_table <- do.call(rbind, lapply(names(completed), function(model) {
    extract_parameter_table(completed[[model]]$result, model)
  }))
  row.names(parameter_table) <- NULL

  geographic_states <- make_state_table(
    areas = prepared_inputs$areas,
    max_range_size = prepared_inputs$max_range_size,
    include_null_range = TRUE
  )
  state_labels <- geographic_states$state
  node_lookup <- make_node_lookup(prepared_inputs$tree_file)

  ancestral_state_probabilities <- do.call(rbind, lapply(names(completed), function(model) {
    extract_ancestral_state_probabilities(
      result = completed[[model]]$result,
      model = model,
      state_labels = state_labels,
      node_lookup = node_lookup
    )
  }))
  row.names(ancestral_state_probabilities) <- NULL

  root_state_probabilities <- do.call(rbind, lapply(names(completed), function(model) {
    extract_root_state_probabilities(
      result = completed[[model]]$result,
      model = model,
      state_labels = state_labels
    )
  }))
  row.names(root_state_probabilities) <- NULL

  node_state_summary <- summarize_top_node_states(ancestral_state_probabilities)

  write_csv_base(geographic_states, file.path(project_paths$tables, "geographic_states.csv"))
  write_csv_base(node_lookup, file.path(project_paths$tables, "tree_nodes.csv"))
  write_csv_base(parameter_table, file.path(project_paths$tables, "model_parameters.csv"))
  write_csv_base(ancestral_state_probabilities, file.path(project_paths$tables, "ancestral_state_probabilities.csv"))
  write_csv_base(root_state_probabilities, file.path(project_paths$tables, "root_state_probabilities.csv"))
  write_csv_base(node_state_summary, file.path(project_paths$tables, "node_state_summary.csv"))

  list(
    geographic_states = geographic_states,
    tree_nodes = node_lookup,
    parameter_table = parameter_table,
    ancestral_state_probabilities = ancestral_state_probabilities,
    root_state_probabilities = root_state_probabilities,
    node_state_summary = node_state_summary
  )
}

extract_parameter_table <- function(result, model) {
  params <- as.data.frame(result$outputs@params_table, stringsAsFactors = FALSE)
  params$parameter <- row.names(params)
  params$model <- model
  params$is_free <- identical_or_na(params$type, "free")

  params <- params[, c(
    "model", "parameter", "type", "is_free", "init", "min", "max",
    "est", "note", "desc"
  )]
  row.names(params) <- NULL
  params
}

extract_ancestral_state_probabilities <- function(result, model, state_labels, node_lookup) {
  matrices <- list(
    branch_top_at_node = result$ML_marginal_prob_each_state_at_branch_top_AT_node,
    branch_bottom_below_node = result$ML_marginal_prob_each_state_at_branch_bottom_below_node
  )

  out <- do.call(rbind, lapply(names(matrices), function(location) {
    probability_matrix_to_long(
      matrix = matrices[[location]],
      model = model,
      location = location,
      state_labels = state_labels,
      node_lookup = node_lookup
    )
  }))
  row.names(out) <- NULL
  out
}

extract_root_state_probabilities <- function(result, model, state_labels) {
  probs <- result$relative_probs_of_each_state_at_bottom_of_root_branch
  if (is.null(probs) || length(probs) == 0L) {
    return(data.frame())
  }

  state_labels <- align_state_labels(state_labels, length(probs))
  out <- data.frame(
    model = model,
    location = "bottom_of_root_branch",
    state_index = seq_along(probs),
    state = state_labels,
    probability = as.numeric(probs),
    stringsAsFactors = FALSE
  )
  out[!is.na(out$probability), , drop = FALSE]
}

probability_matrix_to_long <- function(matrix, model, location, state_labels, node_lookup) {
  if (is.null(matrix) || length(matrix) == 0L) {
    return(data.frame())
  }

  probability_matrix <- as.matrix(matrix)
  state_labels <- align_state_labels(state_labels, ncol(probability_matrix))
  node_indices <- seq_len(nrow(probability_matrix))
  node_rows <- node_lookup[match(node_indices, node_lookup$node_index), , drop = FALSE]

  grid <- expand.grid(
    node_index = node_indices,
    state_index = seq_len(ncol(probability_matrix)),
    KEEP.OUT.ATTRS = FALSE
  )
  probs <- as.vector(probability_matrix)

  out <- data.frame(
    model = model,
    location = location,
    node_index = grid$node_index,
    node_type = node_rows$node_type[match(grid$node_index, node_rows$node_index)],
    node_label = node_rows$node_label[match(grid$node_index, node_rows$node_index)],
    state_index = grid$state_index,
    state = state_labels[grid$state_index],
    probability = as.numeric(probs),
    stringsAsFactors = FALSE
  )
  out[!is.na(out$probability), , drop = FALSE]
}

make_state_labels <- function(areas, max_range_size, include_null_range = TRUE) {
  make_state_table(areas, max_range_size, include_null_range)$state
}

make_state_table <- function(areas, max_range_size, include_null_range = TRUE) {
  max_range_size <- min(as.integer(max_range_size), length(areas))
  states <- data.frame(
    state = character(),
    areas = character(),
    area_count = integer(),
    is_null_range = logical(),
    stringsAsFactors = FALSE
  )

  if (isTRUE(include_null_range)) {
    states <- rbind(states, data.frame(
      state = "null",
      areas = NA_character_,
      area_count = 0L,
      is_null_range = TRUE,
      stringsAsFactors = FALSE
    ))
  }

  for (size in seq_len(max_range_size)) {
    combos <- utils::combn(areas, size, simplify = FALSE)
    states <- rbind(states, do.call(rbind, lapply(combos, function(combo) {
      data.frame(
        state = paste0(combo, collapse = ""),
        areas = paste(combo, collapse = ";"),
        area_count = length(combo),
        is_null_range = FALSE,
        stringsAsFactors = FALSE
      )
    })))
  }

  states$state_index <- seq_len(nrow(states))
  states <- states[, c("state_index", "state", "areas", "area_count", "is_null_range")]
  row.names(states) <- NULL
  states
}

align_state_labels <- function(state_labels, n_states) {
  if (length(state_labels) == n_states) {
    return(state_labels)
  }
  paste0("state_", seq_len(n_states))
}

make_node_lookup <- function(tree_file) {
  tree <- ape::read.tree(tree_file)
  n_tips <- length(tree$tip.label)
  n_nodes <- tree$Nnode
  node_index <- seq_len(n_tips + n_nodes)
  node_type <- ifelse(node_index <= n_tips, "tip", "internal")
  node_label <- paste0("node_", node_index)
  node_label[seq_len(n_tips)] <- tree$tip.label
  root_index <- n_tips + 1L

  parent_node_index <- rep(NA_integer_, length(node_index))
  edge_length <- rep(NA_real_, length(node_index))
  if (!is.null(tree$edge) && nrow(tree$edge) > 0L) {
    child <- tree$edge[, 2L]
    parent_node_index[child] <- tree$edge[, 1L]
    if (!is.null(tree$edge.length)) {
      edge_length[child] <- tree$edge.length
    }
  }

  data.frame(
    node_index = node_index,
    node_type = node_type,
    node_label = node_label,
    parent_node_index = parent_node_index,
    edge_length = edge_length,
    is_root = node_index == root_index,
    stringsAsFactors = FALSE
  )
}

summarize_top_node_states <- function(ancestral_state_probabilities) {
  if (is.null(ancestral_state_probabilities) || nrow(ancestral_state_probabilities) == 0L) {
    return(data.frame())
  }

  split_keys <- paste(
    ancestral_state_probabilities$model,
    ancestral_state_probabilities$location,
    ancestral_state_probabilities$node_index,
    sep = "\r"
  )
  rows <- lapply(split(ancestral_state_probabilities, split_keys), function(x) {
    x <- x[order(-x$probability, x$state_index), , drop = FALSE]
    best <- x[1L, , drop = FALSE]
    data.frame(
      model = best$model,
      location = best$location,
      node_index = best$node_index,
      node_type = best$node_type,
      node_label = best$node_label,
      best_state_index = best$state_index,
      best_state = best$state,
      best_probability = best$probability,
      state_count = nrow(x),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out[order(out$model, out$location, out$node_index), , drop = FALSE]
}

identical_or_na <- function(x, value) {
  out <- x == value
  out[is.na(out)] <- FALSE
  out
}
