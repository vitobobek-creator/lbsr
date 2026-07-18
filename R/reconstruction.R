#' Classify reconstruction constraints
#'
#' Converts documentary evidence into an explicit constraint register. Hard
#' constraints define structural facts that generation must satisfy; soft
#' constraints are statistical targets; diagnostic constraints are retained
#' for audit but are not fitted.
#'
#' @param x An `lbsr_evidence_bundle`.
#' @param respondent_n Intended number of synthetic respondents.
#' @param group_sizes Optional named group sizes.
#' @return A data frame with one row per constraint.
#' @export
classify_constraints <- function(x, respondent_n, group_sizes = NULL) {
  if (!inherits(x, "lbsr_evidence_bundle"))
    stop("x must be an lbsr_evidence_bundle.", call. = FALSE)
  respondent_n <- as.integer(respondent_n)
  if (length(respondent_n) != 1L || is.na(respondent_n) || respondent_n < 2L)
    stop("respondent_n must be one integer greater than 1.", call. = FALSE)
  rows <- list(); k <- 0L
  add <- function(type, target, value, priority, source, tolerance = NA_real_) {
    k <<- k + 1L
    rows[[k]] <<- data.frame(
      constraint_id = sprintf("C%04d", k), constraint_type = type,
      target = as.character(target), value = as.character(value),
      priority = priority, tolerance = tolerance, source = as.character(source),
      stringsAsFactors = FALSE)
  }
  add("respondent_count", "respondents", respondent_n, "hard", "user specification", 0)
  add("scale_bounds", "all_items", "1|5", "hard", "coding-book convention", 0)
  for (i in seq_len(nrow(x$items))) {
    z <- x$items[i, ]
    add("item_mean", z$code, z$mean, "soft", z$internal_source_location, 0.10)
    if ("sd" %in% names(z) && is.finite(suppressWarnings(as.numeric(z$sd))))
      add("item_sd", z$code, z$sd, "soft", z$internal_source_location, 0.15)
  }
  total <- lbsr_total_group_rows(x$group_means)
  for (i in seq_len(nrow(total))) {
    add("composite_mean", total$composite_code[[i]], total$mean[[i]], "soft",
        total$internal_source_location[[i]], 0.10)
  }
  total_labels <- c("skupaj", "total", "all")
  non_total <- x$group_means[
    !tolower(trimws(as.character(x$group_means$pv_ownership_group))) %in% total_labels,
    , drop = FALSE]
  if (nrow(non_total)) for (i in seq_len(nrow(non_total))) {
    add("group_mean", paste(non_total$composite_code[[i]],
                            non_total$pv_ownership_group[[i]], sep = "::"),
        non_total$mean[[i]], "soft", non_total$internal_source_location[[i]], 0.15)
  }
  if (nrow(x$composite_frequencies)) for (i in seq_len(nrow(x$composite_frequencies))) {
    z <- x$composite_frequencies[i, ]
    category_label <- tolower(trimws(iconv(as.character(z$response_category),
                                           from = "", to = "ASCII//TRANSLIT")))
    is_total <- isTRUE(category_label %in% c("skupaj", "total", "all"))
    add(if (is_total) "composite_frequency_total" else "composite_frequency",
        paste(z$composite_code, z$response_category, sep = "::"), z$frequency,
        if (is_total) "diagnostic" else "soft", z$internal_source_location,
        if (is_total) NA_real_ else 0.03)
  }
  if (!is.null(group_sizes)) {
    if (is.null(names(group_sizes)) || any(!nzchar(names(group_sizes))))
      stop("group_sizes must be a named numeric vector.", call. = FALSE)
    for (nm in names(group_sizes))
      add("group_size", nm, as.integer(group_sizes[[nm]]), "hard", "user specification", 0)
  }
  if (nrow(x$hypothesis_tests)) for (i in seq_len(nrow(x$hypothesis_tests))) {
    z <- x$hypothesis_tests[i, ]
    add("legacy_test", z$composite_code, z$statistic, "diagnostic",
        z$internal_source_location, NA_real_)
  }
  do.call(rbind, rows)
}

#' Validate reconstruction constraints
#'
#' @param constraints A register returned by [classify_constraints()].
#' @return An `lbsr_validation` object. Infeasible combinations are errors.
#' @export
validate_constraints <- function(constraints) {
  required <- c("constraint_id", "constraint_type", "target", "value",
                "priority", "tolerance", "source")
  findings <- lbsr_findings()
  missing <- setdiff(required, names(constraints))
  if (length(missing)) return(new_lbsr_validation(add_finding(
    findings, "LBSR-C001", "ERROR", "constraints", paste(missing, collapse = ","),
    message = "Constraint register is missing required fields.")))
  if (anyDuplicated(constraints$constraint_id)) findings <- add_finding(
    findings, "LBSR-C002", "ERROR", "constraints", "constraint_id",
    message = "Constraint identifiers must be unique.")
  bad_priority <- !constraints$priority %in% c("hard", "soft", "diagnostic")
  if (any(bad_priority)) findings <- add_finding(
    findings, "LBSR-C003", "ERROR", "constraints", "priority",
    message = "Priority must be hard, soft, or diagnostic.")
  nrow_c <- constraints$constraint_type == "respondent_count"
  ns <- suppressWarnings(as.integer(constraints$value[nrow_c]))
  if (length(ns) != 1L || is.na(ns) || ns < 2L) findings <- add_finding(
    findings, "LBSR-C004", "ERROR", "constraints", "respondent_count",
    message = "Exactly one valid respondent-count constraint is required.")
  gs <- constraints$constraint_type == "group_size"
  gvals <- suppressWarnings(as.integer(constraints$value[gs]))
  if (any(gs) && any(is.na(gvals) | gvals < 0L)) findings <- add_finding(
    findings, "LBSR-C007", "ERROR", "constraints", "group_size",
    message = "Group sizes must be non-negative integers.")
  if (any(gs) && length(ns) == 1L && !is.na(ns) &&
      !any(is.na(gvals)) && sum(gvals) != ns)
    findings <- add_finding(findings, "LBSR-C005", "ERROR", "constraints", "group_size",
                            message = "Hard group sizes do not sum to respondent count.")
  means <- suppressWarnings(as.numeric(constraints$value[constraints$constraint_type %in%
                                                           c("item_mean", "composite_mean",
                                                             "group_mean")]))
  if (any(!is.finite(means))) findings <- add_finding(
    findings, "LBSR-C008", "ERROR", "constraints", "mean",
    message = "Mean constraints must be finite numeric values.")
  if (any(is.finite(means) & (means < 1 | means > 5))) findings <- add_finding(
    findings, "LBSR-C006", "ERROR", "constraints", "mean",
    message = "A requested mean lies outside the 1--5 scale bounds.")
  new_lbsr_validation(findings)
}

lbsr_discrete_target <- function(n, target, lower = 1L, upper = 5L) {
  target <- max(lower, min(upper, as.numeric(target)))
  total <- as.integer(round(n * target)); base <- total %/% n; rem <- total %% n
  out <- rep(base, n)
  if (rem > 0L) out[seq_len(rem)] <- out[seq_len(rem)] + 1L
  pmax(lower, pmin(upper, out))
}

#' Generate a seed-controlled synthetic reconstruction
#'
#' Generates integer Likert records that match item means to the closest total
#' attainable at the requested sample size. Rows are shuffled per item using a
#' local, restored random-number state. Output is always labelled synthetic.
#'
#' @param x An `lbsr_evidence_bundle`.
#' @param respondent_n Number of synthetic records.
#' @param seed Integer random seed.
#' @param group_sizes Optional named group sizes summing to `respondent_n`.
#' @return An `lbsr_reconstruction` object.
#' @export
reconstruct_synthetic <- function(x, respondent_n, seed = 2026L, group_sizes = NULL) {
  constraints <- classify_constraints(x, respondent_n, group_sizes)
  validation <- validate_constraints(constraints)
  if (!validation$valid) stop("Constraints are infeasible; inspect validate_constraints().",
                              call. = FALSE)
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  n <- as.integer(respondent_n)
  dat <- data.frame(respondent_id = sprintf("SYN%04d", seq_len(n)),
                    stringsAsFactors = FALSE)
  if (!is.null(group_sizes))
    dat$group <- sample(rep(names(group_sizes), as.integer(group_sizes)), n)
  for (i in seq_len(nrow(x$items))) {
    code <- as.character(x$items$code[[i]])
    vals <- lbsr_discrete_target(n, x$items$mean[[i]])
    dat[[code]] <- sample(vals, n, replace = FALSE)
  }
  out <- structure(list(
    data = dat, constraints = constraints, validation = validation,
    metadata = list(data_status = "synthetic_analytical_reconstruction",
                    historical_respondents_recovered = FALSE,
                    respondent_n = n, seed = as.integer(seed),
                    generated_at = lbsr_now(), generator = "balanced_integer_means_v1")
  ), class = "lbsr_reconstruction")
  out$fidelity <- score_fidelity(out, x)
  out
}

#' Score reconstruction fidelity
#'
#' @param reconstruction An `lbsr_reconstruction`.
#' @param evidence The source `lbsr_evidence_bundle`.
#' @return Row-level errors plus an overall summary stored as attributes.
#' @export
score_fidelity <- function(reconstruction, evidence) {
  if (!inherits(reconstruction, "lbsr_reconstruction"))
    stop("reconstruction must be an lbsr_reconstruction.", call. = FALSE)
  rows <- lapply(seq_len(nrow(evidence$items)), function(i) {
    code <- as.character(evidence$items$code[[i]])
    target <- suppressWarnings(as.numeric(evidence$items$mean[[i]]))
    achieved <- mean(reconstruction$data[[code]], na.rm = TRUE)
    data.frame(metric = "item_mean", target_id = code, target = target,
               achieved = achieved, absolute_error = abs(achieved - target),
               tolerance = 0.10, pass = abs(achieved - target) <= 0.10,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  attr(out, "summary") <- data.frame(
    metric = "item_mean", n_targets = nrow(out),
    pass_rate = mean(out$pass), mean_absolute_error = mean(out$absolute_error),
    max_absolute_error = max(out$absolute_error), stringsAsFactors = FALSE)
  out
}

#' @export
print.lbsr_reconstruction <- function(x, ...) {
  cat("<lbsr_reconstruction>\n")
  cat("  Status:      synthetic analytical reconstruction\n")
  cat("  Respondents: ", nrow(x$data), "\n", sep = "")
  cat("  Items:       ", sum(!names(x$data) %in% c("respondent_id", "group")), "\n", sep = "")
  cat("  Seed:        ", x$metadata$seed, "\n", sep = "")
  cat("  Fidelity:    ", round(mean(x$fidelity$pass) * 100, 1), "% targets passed\n", sep = "")
  invisible(x)
}
