lbsr_composite_statistics <- function(items, digits = 3L) {
  items$mean <- suppressWarnings(as.numeric(items$mean))
  items$sd <- suppressWarnings(as.numeric(items$sd))
  code <- paste(items$tpb_block_code, items$dimension_code, sep = "_")
  split_items <- split(items, code)
  do.call(rbind, lapply(names(split_items), function(k) {
    z <- split_items[[k]]
    data.frame(
      composite_code = k,
      item_count = nrow(z),
      item_mean_average = mean(z$mean, na.rm = TRUE),
      mean_signature = paste(format(round(z$mean, digits), nsmall = digits), collapse = "|"),
      sd_signature = paste(format(round(z$sd, digits), nsmall = digits), collapse = "|"),
      stringsAsFactors = FALSE
    )
  }))
}

lbsr_total_group_rows <- function(group_means,
                                  total_group_labels = c("skupaj", "total", "all")) {
  labels <- tolower(trimws(as.character(group_means$pv_ownership_group)))
  group_means[labels %in% total_group_labels, , drop = FALSE]
}

detect_source_conflicts <- function(x, mean_tolerance = 0.10,
                                    duplicate_digits = 3L,
                                    claimed_item_counts = NULL) {
  if (!inherits(x, "lbsr_evidence_bundle")) {
    stop("x must be an lbsr_evidence_bundle.", call. = FALSE)
  }
  stats <- lbsr_composite_statistics(x$items, digits = duplicate_digits)
  total <- lbsr_total_group_rows(x$group_means)
  total$mean <- suppressWarnings(as.numeric(total$mean))
  conflicts <- new_lbsr_diagnostics()$conflicts
  duplicates <- new_lbsr_diagnostics()$duplicates
  issues <- lbsr_findings()
  if (nrow(total)) {
    cmp <- merge(stats, total[, c("composite_code", "mean", "internal_source_location")],
                 by = "composite_code", all = FALSE)
    for (i in seq_len(nrow(cmp))) {
      delta <- abs(cmp$item_mean_average[[i]] - cmp$mean[[i]])
      if (is.finite(delta) && delta > mean_tolerance) {
        id <- paste0("CON-MEAN-", sprintf("%03d", nrow(conflicts) + 1L))
        conflicts <- rbind(conflicts, data.frame(
          conflict_id = id, conflict_type = "item_vs_composite_mean",
          target = cmp$composite_code[[i]],
          value_a = format(cmp$item_mean_average[[i]], digits = 6),
          source_a = "Mean of reported item means",
          value_b = format(cmp$mean[[i]], digits = 6),
          source_b = as.character(cmp$internal_source_location[[i]]),
          difference = delta, tolerance = mean_tolerance,
          recommended_status = "CONFLICTED",
          message = "Item-derived mean and reported total composite mean exceed tolerance.",
          stringsAsFactors = FALSE
        ))
        issues <- add_finding(issues, "LBSR-E001", "MAJOR", "cross_source", "mean",
          message = paste(id, cmp$composite_code[[i]], "mean conflict; difference", round(delta, 3)))
      }
    }
  }
  signatures <- paste(stats$item_count, stats$mean_signature, stats$sd_signature, sep = "::")
  groups <- split(seq_len(nrow(stats)), signatures)
  groups <- groups[lengths(groups) > 1L]
  for (g in groups) {
    for (pair in combn(g, 2, simplify = FALSE)) {
      a <- stats$composite_code[[pair[[1]]]]; b <- stats$composite_code[[pair[[2]]]]
      id <- paste0("DUP-STAT-", sprintf("%03d", nrow(duplicates) + 1L))
      duplicates <- rbind(duplicates, data.frame(
        duplicate_id = id, composite_a = a, composite_b = b,
        signature = signatures[[pair[[1]]]], recommended_status = "REVIEW_REQUIRED",
        message = "Different composites have identical ordered item mean and SD patterns.",
        stringsAsFactors = FALSE
      ))
      issues <- add_finding(issues, "LBSR-E002", "WARNING", "items", "mean_sd_pattern",
        message = paste(id, "identical statistical pattern for", a, "and", b))
    }
  }
  if (!is.null(claimed_item_counts)) {
    claims <- unique(as.integer(claimed_item_counts[!is.na(claimed_item_counts)]))
    actual <- nrow(x$items)
    for (claim in claims[claims != actual]) {
      id <- paste0("CON-COUNT-", sprintf("%03d", nrow(conflicts) + 1L))
      conflicts <- rbind(conflicts, data.frame(
        conflict_id = id, conflict_type = "inventory_item_count",
        target = "manifest_indicator_count", value_a = as.character(actual),
        source_a = "Imported master coding book", value_b = as.character(claim),
        source_b = "External documentation claim", difference = abs(actual - claim),
        tolerance = 0, recommended_status = "CONFLICTED",
        message = "Documented item-count claim differs from the imported inventory.",
        stringsAsFactors = FALSE
      ))
      issues <- add_finding(issues, "LBSR-M003", "MAJOR", "inventory", "item_count",
        message = paste(id, "imported", actual, "items but documentation claims", claim))
    }
  }
  new_lbsr_diagnostics(
    conflicts = conflicts, duplicates = duplicates, issues = issues,
    metadata = list(mean_tolerance = mean_tolerance,
                    duplicate_digits = duplicate_digits, generated_at = lbsr_now())
  )
}

diagnose_evidence <- function(x, respondent_n = NULL,
                              claimed_item_counts = NULL,
                              mean_tolerance = 0.10) {
  lbsr_bind_diagnostics(
    diagnose_sample_sizes(x, respondent_n = respondent_n),
    detect_source_conflicts(x, mean_tolerance = mean_tolerance,
                            claimed_item_counts = claimed_item_counts)
  )
}

run_lbsr_diagnostics <- diagnose_evidence

