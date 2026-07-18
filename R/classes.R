new_lbsr_project <- function(path, manifest) {
  structure(
    list(path = normalizePath(path, winslash = "/", mustWork = FALSE),
         manifest = manifest),
    class = "lbsr_project"
  )
}

print.lbsr_project <- function(x, ...) {
  cat("<lbsr_project>\n")
  cat("  ID:      ", x$manifest$project_id, "\n", sep = "")
  cat("  Title:   ", x$manifest$title, "\n", sep = "")
  cat("  Status:  ", x$manifest$status, "\n", sep = "")
  cat("  Path:    ", x$path, "\n", sep = "")
  cat("  Sources: ", length(x$manifest$sources %||% list()), "\n", sep = "")
  invisible(x)
}

new_lbsr_evidence_bundle <- function(items, composite_frequencies, group_means,
                                     hypothesis_tests, reconstruction_plan,
                                     metadata = list(), validation = NULL) {
  structure(
    list(
      items = items,
      composite_frequencies = composite_frequencies,
      group_means = group_means,
      hypothesis_tests = hypothesis_tests,
      reconstruction_plan = reconstruction_plan,
      metadata = metadata,
      validation = validation
    ),
    class = "lbsr_evidence_bundle"
  )
}

print.lbsr_evidence_bundle <- function(x, ...) {
  cat("<lbsr_evidence_bundle>\n")
  cat("  Items:                ", nrow(x$items), "\n", sep = "")
  cat("  Composite frequencies:", nrow(x$composite_frequencies), "\n")
  cat("  Group means:          ", nrow(x$group_means), "\n", sep = "")
  cat("  Hypothesis tests:     ", nrow(x$hypothesis_tests), "\n", sep = "")
  if (!is.null(x$validation)) {
    cat("  Valid:                 ", x$validation$valid, "\n", sep = "")
    cat("  Findings:              ", nrow(x$validation$findings), "\n", sep = "")
  }
  invisible(x)
}

new_lbsr_validation <- function(findings = NULL) {
  if (is.null(findings)) {
    findings <- data.frame(
      code = character(), severity = character(), table = character(),
      field = character(), row = integer(), message = character(),
      stringsAsFactors = FALSE
    )
  }
  stopifnot(is.data.frame(findings))
  severity <- toupper(findings$severity %||% character())
  valid <- !any(severity %in% c("ERROR", "CRITICAL"))
  structure(list(valid = valid, findings = findings), class = "lbsr_validation")
}

print.lbsr_validation <- function(x, ...) {
  cat("<lbsr_validation>\n")
  cat("  Valid:    ", x$valid, "\n", sep = "")
  cat("  Findings: ", nrow(x$findings), "\n", sep = "")
  if (nrow(x$findings)) print(validation_summary(x), row.names = FALSE)
  invisible(x)
}

validation_summary <- function(x) {
  if (!inherits(x, "lbsr_validation")) {
    stop("x must be an lbsr_validation object.", call. = FALSE)
  }
  if (!nrow(x$findings)) {
    return(data.frame(severity = character(), n = integer()))
  }
  out <- as.data.frame(table(toupper(x$findings$severity)), stringsAsFactors = FALSE)
  names(out) <- c("severity", "n")
  out[order(match(out$severity, c("CRITICAL", "ERROR", "MAJOR", "WARNING", "INFO"))), ]
}

