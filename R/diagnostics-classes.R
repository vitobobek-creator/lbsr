new_lbsr_diagnostics <- function(sample_sizes = NULL, conflicts = NULL,
                                 duplicates = NULL, issues = NULL,
                                 metadata = list()) {
  empty_sample <- data.frame(
    composite_code = character(), group = character(), reported_n = numeric(),
    item_multiplicity = integer(), inferred_respondents = numeric(),
    count_type = character(), confidence = character(),
    stringsAsFactors = FALSE
  )
  empty_conflicts <- data.frame(
    conflict_id = character(), conflict_type = character(), target = character(),
    value_a = character(), source_a = character(), value_b = character(),
    source_b = character(), difference = numeric(), tolerance = numeric(),
    recommended_status = character(), message = character(),
    stringsAsFactors = FALSE
  )
  empty_duplicates <- data.frame(
    duplicate_id = character(), composite_a = character(), composite_b = character(),
    signature = character(), recommended_status = character(), message = character(),
    stringsAsFactors = FALSE
  )
  structure(list(
    sample_sizes = sample_sizes %||% empty_sample,
    conflicts = conflicts %||% empty_conflicts,
    duplicates = duplicates %||% empty_duplicates,
    issues = issues %||% lbsr_findings(),
    metadata = metadata
  ), class = "lbsr_diagnostics")
}

print.lbsr_diagnostics <- function(x, ...) {
  cat("<lbsr_diagnostics>\n")
  cat("  Sample-size records: ", nrow(x$sample_sizes), "\n", sep = "")
  cat("  Conflicts:           ", nrow(x$conflicts), "\n", sep = "")
  cat("  Duplicate patterns:  ", nrow(x$duplicates), "\n", sep = "")
  cat("  Issues:               ", nrow(x$issues), "\n", sep = "")
  if (nrow(x$issues)) {
    counts <- as.data.frame(table(x$issues$severity), stringsAsFactors = FALSE)
    names(counts) <- c("severity", "n")
    print(counts, row.names = FALSE)
  }
  invisible(x)
}

lbsr_bind_diagnostics <- function(...) {
  xs <- list(...)
  new_lbsr_diagnostics(
    sample_sizes = do.call(rbind, lapply(xs, `[[`, "sample_sizes")),
    conflicts = do.call(rbind, lapply(xs, `[[`, "conflicts")),
    duplicates = do.call(rbind, lapply(xs, `[[`, "duplicates")),
    issues = do.call(rbind, lapply(xs, `[[`, "issues")),
    metadata = list(generated_at = lbsr_now(), components = length(xs))
  )
}

