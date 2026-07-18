lbsr_ensemble_grid <- function(seeds, reliability_values, correlation_values) {
  grid <- expand.grid(
    seed = as.integer(seeds), target_reliability = as.numeric(reliability_values),
    target_correlation = as.numeric(correlation_values),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$run_id <- sprintf("RUN%04d", seq_len(nrow(grid)))
  grid[, c("run_id", "seed", "target_reliability", "target_correlation")]
}

lbsr_run_score <- function(fidelity) {
  if (!nrow(fidelity)) return(c(pass_rate = NA_real_, normalized_loss = Inf,
                                mean_absolute_error = Inf))
  denom <- ifelse(is.finite(fidelity$tolerance) & fidelity$tolerance > 0,
                  fidelity$tolerance, 1)
  target_loss <- fidelity$absolute_error / denom
  family_loss <- tapply(target_loss, fidelity$metric, mean, na.rm = TRUE)
  c(pass_rate = mean(fidelity$pass, na.rm = TRUE),
    normalized_loss = mean(family_loss, na.rm = TRUE),
    mean_absolute_error = mean(fidelity$absolute_error, na.rm = TRUE))
}

#' Generate a sensitivity ensemble
#'
#' Runs the multivariate reconstruction over a Cartesian grid of random seeds,
#' reliability assumptions, and correlation assumptions. Synthetic rows are
#' never matched or interpreted as the same individuals across runs.
#'
#' @param x An `lbsr_evidence_bundle`.
#' @param respondent_n Number of synthetic records per run.
#' @param seeds Integer seeds.
#' @param reliability_values Reliability assumptions in `(0, 1)`.
#' @param correlation_values Correlation assumptions in `[0, 1)`.
#' @param group_sizes Optional named group sizes.
#' @param group_tolerance Group-mean tolerance.
#' @param max_swaps Maximum calibration swaps per target.
#' @param max_runs Safety limit for the parameter grid.
#' @param keep_reconstructions Whether to retain complete run objects.
#' @return An `lbsr_sensitivity` object.
#' @export
reconstruct_ensemble <- function(x, respondent_n, seeds = 1:10,
                                 reliability_values = c(0.60, 0.70, 0.80),
                                 correlation_values = c(0.15, 0.25, 0.35),
                                 group_sizes = NULL, group_tolerance = 0.15,
                                 max_swaps = 10000L, max_runs = 100L,
                                 keep_reconstructions = TRUE) {
  if (!inherits(x, "lbsr_evidence_bundle"))
    stop("x must be an lbsr_evidence_bundle.", call. = FALSE)
  seed_values <- suppressWarnings(as.numeric(seeds))
  if (!length(seed_values) || any(!is.finite(seed_values) | seed_values < 0 |
                                   seed_values > .Machine$integer.max |
                                   seed_values != floor(seed_values)))
    stop("seeds must contain non-negative integers within the R integer range.",
         call. = FALSE)
  if (!length(reliability_values) || any(!is.finite(reliability_values) |
                                          reliability_values <= 0 |
                                          reliability_values >= 1))
    stop("reliability_values must lie strictly between 0 and 1.", call. = FALSE)
  if (!length(correlation_values) || any(!is.finite(correlation_values) |
                                          correlation_values < 0 |
                                          correlation_values >= 1))
    stop("correlation_values must lie in [0, 1).", call. = FALSE)
  if (length(max_runs) != 1L || !is.finite(max_runs) || max_runs < 1L)
    stop("max_runs must be one positive integer.", call. = FALSE)
  if (length(keep_reconstructions) != 1L || is.na(keep_reconstructions) ||
      !is.logical(keep_reconstructions))
    stop("keep_reconstructions must be TRUE or FALSE.", call. = FALSE)
  grid <- lbsr_ensemble_grid(unique(seeds), unique(reliability_values),
                             unique(correlation_values))
  if (nrow(grid) > as.integer(max_runs))
    stop("Sensitivity grid exceeds max_runs (", nrow(grid), " > ", max_runs, ").",
         call. = FALSE)
  runs <- vector("list", nrow(grid)); fidelities <- vector("list", nrow(grid))
  registry <- grid; registry$status <- "pending"; registry$error <- NA_character_
  registry$pass_rate <- NA_real_; registry$normalized_loss <- NA_real_
  registry$mean_absolute_error <- NA_real_
  for (i in seq_len(nrow(grid))) {
    fit <- tryCatch(reconstruct_multivariate(
      x, respondent_n = respondent_n, seed = grid$seed[[i]], group_sizes = group_sizes,
      target_reliability = grid$target_reliability[[i]],
      target_correlation = grid$target_correlation[[i]],
      group_tolerance = group_tolerance, max_swaps = max_swaps),
      error = function(e) e)
    if (inherits(fit, "error")) {
      registry$status[[i]] <- "failed"; registry$error[[i]] <- conditionMessage(fit)
      next
    }
    registry$status[[i]] <- "succeeded"
    f <- fit$fidelity; f$run_id <- grid$run_id[[i]]
    f$seed <- grid$seed[[i]]; f$target_reliability_assumption <- grid$target_reliability[[i]]
    f$target_correlation_assumption <- grid$target_correlation[[i]]
    fidelities[[i]] <- f
    score <- lbsr_run_score(f)
    registry$pass_rate[[i]] <- score[["pass_rate"]]
    registry$normalized_loss[[i]] <- score[["normalized_loss"]]
    registry$mean_absolute_error[[i]] <- score[["mean_absolute_error"]]
    if (isTRUE(keep_reconstructions)) runs[[i]] <- fit
  }
  successful <- registry$status == "succeeded"
  if (!any(successful)) stop("Every ensemble run failed.", call. = FALSE)
  all_fidelity <- do.call(rbind, fidelities[successful])
  out <- structure(list(
    registry = registry, fidelity = all_fidelity,
    reconstructions = if (isTRUE(keep_reconstructions)) runs else NULL,
    metadata = list(data_status = "synthetic_sensitivity_ensemble",
      historical_respondents_recovered = FALSE, respondent_n = as.integer(respondent_n),
      run_count = nrow(grid), successful_runs = sum(successful),
      comparison_unit = "aggregate_targets_not_synthetic_rows",
      generated_at = lbsr_now())
  ), class = "lbsr_sensitivity")
  out$summary <- summarize_sensitivity(out)
  out
}

#' Summarize sensitivity and uncertainty
#'
#' @param x An `lbsr_sensitivity` object.
#' @return A list containing target stability, run ranking, and parameter effects.
#' @export
summarize_sensitivity <- function(x) {
  if (!inherits(x, "lbsr_sensitivity"))
    stop("x must be an lbsr_sensitivity object.", call. = FALSE)
  f <- x$fidelity
  keys <- unique(f[, c("metric", "target_id"), drop = FALSE])
  stability <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
    z <- f[f$metric == keys$metric[[i]] & f$target_id == keys$target_id[[i]], , drop = FALSE]
    achieved_range <- diff(range(z$achieved, na.rm = TRUE))
    tolerance <- stats::median(z$tolerance, na.rm = TRUE)
    pass_rate <- mean(z$pass, na.rm = TRUE)
    classification <- if (isTRUE(pass_rate == 1)) "stable_pass" else if (
      isTRUE(pass_rate == 0) && is.finite(achieved_range) && achieved_range <= tolerance)
      "stable_fail" else "assumption_sensitive"
    data.frame(metric = keys$metric[[i]], target_id = keys$target_id[[i]],
      runs = nrow(z), target_min = min(z$target, na.rm = TRUE),
      target_max = max(z$target, na.rm = TRUE), achieved_mean = mean(z$achieved, na.rm = TRUE),
      achieved_sd = stats::sd(z$achieved, na.rm = TRUE),
      achieved_min = min(z$achieved, na.rm = TRUE),
      achieved_max = max(z$achieved, na.rm = TRUE), achieved_range = achieved_range,
      pass_rate = pass_rate, classification = classification, stringsAsFactors = FALSE)
  }))
  ranking <- x$registry[x$registry$status == "succeeded", , drop = FALSE]
  ranking <- ranking[order(ranking$normalized_loss, -ranking$pass_rate,
                           ranking$run_id), , drop = FALSE]
  ranking$rank <- seq_len(nrow(ranking))
  effects <- do.call(rbind, lapply(c("target_reliability", "target_correlation"), function(v) {
    split_r <- split(ranking, ranking[[v]])
    do.call(rbind, lapply(names(split_r), function(level) {
      z <- split_r[[level]]
      data.frame(parameter = v, value = as.numeric(level), runs = nrow(z),
        mean_pass_rate = mean(z$pass_rate), mean_normalized_loss = mean(z$normalized_loss),
        sd_normalized_loss = stats::sd(z$normalized_loss), stringsAsFactors = FALSE)
    }))
  }))
  list(target_stability = stability, run_ranking = ranking,
       parameter_effects = effects,
       classification_counts = as.data.frame(table(stability$classification),
                                               stringsAsFactors = FALSE))
}

#' Select a reconstruction by a declared rule
#'
#' @param x An `lbsr_sensitivity` object with retained reconstructions.
#' @param rule Selection rule: minimum normalized loss or maximum pass rate.
#' @return The selected `lbsr_reconstruction`, annotated with selection metadata.
#' @export
select_sensitivity_run <- function(x, rule = c("min_normalized_loss", "max_pass_rate")) {
  if (!inherits(x, "lbsr_sensitivity"))
    stop("x must be an lbsr_sensitivity object.", call. = FALSE)
  if (is.null(x$reconstructions))
    stop("Reconstructions were not retained; rerun with keep_reconstructions = TRUE.",
         call. = FALSE)
  rule <- match.arg(rule)
  z <- x$registry[x$registry$status == "succeeded", , drop = FALSE]
  ord <- if (rule == "min_normalized_loss")
    order(z$normalized_loss, -z$pass_rate, z$run_id) else
    order(-z$pass_rate, z$normalized_loss, z$run_id)
  chosen <- z[ord[[1]], , drop = FALSE]
  idx <- match(chosen$run_id, x$registry$run_id)
  out <- x$reconstructions[[idx]]
  out$metadata$ensemble_run_id <- chosen$run_id[[1]]
  out$metadata$selection_rule <- rule
  out$metadata$selection_is_methodological_truth <- FALSE
  out
}

lbsr_markdown_table <- function(x, digits = 4L) {
  if (!nrow(x)) return("_No records._")
  z <- x
  z[] <- lapply(z, function(v) {
    if (is.numeric(v)) format(round(v, digits), trim = TRUE, scientific = FALSE) else
      gsub("\\|", "\\\\|", as.character(v))
  })
  header <- paste0("| ", paste(names(z), collapse = " | "), " |")
  rule <- paste0("| ", paste(rep("---", ncol(z)), collapse = " | "), " |")
  body <- apply(z, 1L, function(v) paste0("| ", paste(v, collapse = " | "), " |"))
  paste(c(header, rule, body), collapse = "\n")
}

#' Write a manuscript-ready sensitivity report
#'
#' @param x An `lbsr_sensitivity` object.
#' @param file Output Markdown path.
#' @param title Report title.
#' @return The normalized path, invisibly.
#' @export
write_sensitivity_report <- function(x, file,
                                     title = "LBSR sensitivity analysis report") {
  if (!inherits(x, "lbsr_sensitivity"))
    stop("x must be an lbsr_sensitivity object.", call. = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  s <- x$summary; best <- s$run_ranking[1, , drop = FALSE]
  counts <- s$classification_counts
  lines <- c(
    paste0("# ", title), "",
    "## Status and interpretation", "",
    paste0("This report summarizes ", x$metadata$successful_runs, " successful runs from ",
           x$metadata$run_count, " planned synthetic reconstructions."),
    "The ensemble represents assumption uncertainty; it does not recover historical respondents.",
    "Synthetic rows are not matched across runs. Comparisons use aggregate evidence targets only.", "",
    "## Declared assumption grid", "",
    lbsr_markdown_table(unique(x$registry[, c("seed", "target_reliability",
                                               "target_correlation")])), "",
    "## Preferred run under the declared loss rule", "",
    lbsr_markdown_table(best[, c("run_id", "seed", "target_reliability",
                                  "target_correlation", "pass_rate",
                                  "normalized_loss")]), "",
    "The preferred run minimizes tolerance-normalized loss averaged first within each metric family and then equally across families; this is a transparent selection rule, not methodological truth.", "",
    "## Stability classification", "", lbsr_markdown_table(counts), "",
    "- `stable_pass`: every run meets the target tolerance.",
    "- `stable_fail`: every run fails and variation across assumptions is no larger than tolerance.",
    "- `assumption_sensitive`: conclusions change across seeds or modelling assumptions.", "",
    "## Parameter effects", "", lbsr_markdown_table(s$parameter_effects), "",
    "## Target-level uncertainty", "", lbsr_markdown_table(s$target_stability), "",
    "## Reporting safeguard", "",
    "Results should be reported as sensitivity of synthetic analytical reconstructions to declared assumptions. They must not be described as estimates from recovered respondent-level data.", "",
    paste0("Generated: ", lbsr_now()))
  writeLines(lines, file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}

#' @export
print.lbsr_sensitivity <- function(x, ...) {
  cat("<lbsr_sensitivity>\n")
  cat("  Status:          synthetic sensitivity ensemble\n")
  cat("  Planned runs:    ", x$metadata$run_count, "\n", sep = "")
  cat("  Successful runs: ", x$metadata$successful_runs, "\n", sep = "")
  cat("  Stable passes:   ", sum(x$summary$target_stability$classification ==
                                 "stable_pass"), "\n", sep = "")
  cat("  Sensitive:       ", sum(x$summary$target_stability$classification ==
                                 "assumption_sensitive"), "\n", sep = "")
  invisible(x)
}
