lbsr_rng_scope <- function(seed) {
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(as.integer(seed))
  function() {
    if (is.null(old)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old, envir = .GlobalEnv)
  }
}

lbsr_composite_codes <- function(items) {
  paste(items$tpb_block_code, items$dimension_code, sep = "_")
}

lbsr_response_category <- function(x) {
  raw <- trimws(tolower(iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")))
  numeric_value <- suppressWarnings(as.integer(raw))
  labels <- c(
    "sploh se ne strinjam" = 1L, "strongly disagree" = 1L,
    "se ne strinjam" = 2L, "disagree" = 2L,
    "niti niti" = 3L, "neither agree nor disagree" = 3L, "neutral" = 3L,
    "se strinjam" = 4L, "agree" = 4L,
    "se popolnoma strinjam" = 5L, "sploh strinjam" = 5L,
    "strongly agree" = 5L)
  matched <- unname(labels[raw])
  numeric_value[is.na(numeric_value)] <- matched[is.na(numeric_value)]
  numeric_value
}

lbsr_frequency_probabilities <- function(frequencies, composite) {
  z <- frequencies[as.character(frequencies$composite_code) == composite, , drop = FALSE]
  category <- lbsr_response_category(z$response_category)
  value <- suppressWarnings(as.numeric(z$frequency))
  keep <- category %in% 1:5 & is.finite(value) & value >= 0
  out <- rep(NA_real_, 5L)
  if (!any(keep) || sum(value[keep]) <= 0) return(out)
  agg <- tapply(value[keep], category[keep], sum)
  out[as.integer(names(agg))] <- agg / sum(agg)
  out[is.na(out)] <- 0
  out
}

lbsr_largest_remainder <- function(n, probabilities) {
  raw <- n * probabilities / sum(probabilities)
  counts <- floor(raw)
  left <- n - sum(counts)
  if (left > 0L) {
    take <- order(raw - counts, decreasing = TRUE)[seq_len(left)]
    counts[take] <- counts[take] + 1L
  }
  rep(seq_along(counts), counts)
}

lbsr_calibrate_sum <- function(values, target_sum) {
  target_sum <- as.integer(round(target_sum))
  delta <- target_sum - sum(values)
  while (delta != 0L) {
    eligible <- if (delta > 0L) which(values < 5L) else which(values > 1L)
    if (!length(eligible)) break
    j <- sample(eligible, 1L)
    values[[j]] <- values[[j]] + sign(delta)
    delta <- target_sum - sum(values)
  }
  values
}

lbsr_alpha_target_r <- function(alpha, k) {
  if (k < 2L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) return(NA_real_)
  alpha / (k - alpha * (k - 1))
}

lbsr_named_target <- function(x, name, default = NA_real_) {
  if (length(x) == 1L && is.null(names(x))) return(as.numeric(x))
  if (!is.null(names(x)) && name %in% names(x)) return(as.numeric(x[[name]]))
  default
}

#' Cronbach's alpha for a numeric item matrix
#'
#' @param x A matrix or data frame with items in columns.
#' @return Cronbach's alpha, or `NA_real_` when it cannot be estimated.
#' @export
cronbach_alpha <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  k <- ncol(x)
  if (k < 2L || nrow(x) < 2L) return(NA_real_)
  item_var <- apply(x, 2L, stats::var, na.rm = TRUE)
  total_var <- stats::var(rowSums(x, na.rm = FALSE), na.rm = TRUE)
  if (!is.finite(total_var) || total_var <= 0) return(NA_real_)
  k / (k - 1) * (1 - sum(item_var, na.rm = TRUE) / total_var)
}

lbsr_group_calibrate <- function(dat, item_codes, group, target, tolerance,
                                 max_swaps = 10000L) {
  inside <- which(dat$group == group); outside <- which(dat$group != group)
  if (!length(inside) || !length(outside) || !length(item_codes)) return(dat)
  swaps <- 0L
  current <- mean(as.matrix(dat[inside, item_codes, drop = FALSE]))
  while (is.finite(current) && abs(current - target) > tolerance && swaps < max_swaps) {
    code <- sample(item_codes, 1L)
    if (current < target) {
      a <- inside[which.min(dat[inside, code])]
      b <- outside[which.max(dat[outside, code])]
      if (dat[b, code] <= dat[a, code]) break
    } else {
      a <- inside[which.max(dat[inside, code])]
      b <- outside[which.min(dat[outside, code])]
      if (dat[b, code] >= dat[a, code]) break
    }
    tmp <- dat[a, code]; dat[a, code] <- dat[b, code]; dat[b, code] <- tmp
    swaps <- swaps + 1L
    current <- mean(as.matrix(dat[inside, item_codes, drop = FALSE]))
  }
  dat
}

#' Generate a multivariate synthetic reconstruction
#'
#' Uses reported composite category frequencies as proportional distribution
#' targets, calibrates item totals, induces within-composite association through
#' a Gaussian rank copula, and optionally calibrates matched group means.
#'
#' @param x An `lbsr_evidence_bundle`.
#' @param respondent_n Number of synthetic records.
#' @param seed Integer seed.
#' @param group_sizes Optional named group sizes; names should match evidence.
#' @param target_reliability Scalar or named vector of alpha evaluation targets.
#' @param target_correlation Within-composite rank-correlation generation assumption.
#' @param group_tolerance Absolute group-mean calibration tolerance.
#' @param max_swaps Maximum swaps per group-composite target.
#' @return An `lbsr_reconstruction` with multidimensional fidelity results.
#' @export
reconstruct_multivariate <- function(x, respondent_n, seed = 2026L,
                                      group_sizes = NULL,
                                      target_reliability = 0.70,
                                      target_correlation = 0.25,
                                      group_tolerance = 0.15,
                                      max_swaps = 10000L) {
  rel <- suppressWarnings(as.numeric(target_reliability))
  if (length(rel) && any(!is.finite(rel) | rel <= 0 | rel >= 1))
    stop("target_reliability values must lie strictly between 0 and 1.", call. = FALSE)
  if (length(target_correlation) != 1L || !is.finite(target_correlation) ||
      target_correlation < 0 || target_correlation >= 1)
    stop("target_correlation must lie in [0, 1).", call. = FALSE)
  if (length(group_tolerance) != 1L || !is.finite(group_tolerance) || group_tolerance < 0)
    stop("group_tolerance must be one non-negative number.", call. = FALSE)
  if (length(max_swaps) != 1L || is.na(max_swaps) || max_swaps < 0)
    stop("max_swaps must be one non-negative integer.", call. = FALSE)
  constraints <- classify_constraints(x, respondent_n, group_sizes)
  validation <- validate_constraints(constraints)
  if (!validation$valid) stop("Constraints are infeasible; inspect validate_constraints().",
                              call. = FALSE)
  restore_rng <- lbsr_rng_scope(seed); on.exit(restore_rng(), add = TRUE)
  n <- as.integer(respondent_n)
  dat <- data.frame(respondent_id = sprintf("SYN%04d", seq_len(n)),
                    stringsAsFactors = FALSE)
  if (!is.null(group_sizes))
    dat$group <- sample(rep(names(group_sizes), as.integer(group_sizes)), n)
  composites <- lbsr_composite_codes(x$items)
  for (comp in unique(composites)) {
    idx <- which(composites == comp); k <- length(idx)
    r <- max(0, min(0.95, as.numeric(target_correlation)))
    shared <- stats::rnorm(n)
    probs <- lbsr_frequency_probabilities(x$composite_frequencies, comp)
    for (j in idx) {
      vals <- if (all(is.finite(probs)) && sum(probs) > 0)
        lbsr_largest_remainder(n, probs) else
        lbsr_discrete_target(n, x$items$mean[[j]])
      vals <- lbsr_calibrate_sum(vals, n * as.numeric(x$items$mean[[j]]))
      latent <- sqrt(r) * shared + sqrt(1 - r) * stats::rnorm(n)
      arranged <- numeric(n); arranged[order(latent)] <- sort(vals)
      dat[[as.character(x$items$code[[j]])]] <- as.integer(arranged)
    }
  }
  if (!is.null(group_sizes) && nrow(x$group_means)) {
    total_labels <- c("skupaj", "total", "all")
    gm <- x$group_means[!tolower(trimws(as.character(x$group_means$pv_ownership_group))) %in%
                          total_labels, , drop = FALSE]
    for (i in seq_len(nrow(gm))) {
      g <- as.character(gm$pv_ownership_group[[i]])
      if (!g %in% names(group_sizes)) next
      codes <- as.character(x$items$code[composites == gm$composite_code[[i]]])
      dat <- lbsr_group_calibrate(dat, codes, g, as.numeric(gm$mean[[i]]),
                                  group_tolerance, max_swaps)
    }
  }
  for (comp in unique(composites)) {
    alpha <- lbsr_named_target(target_reliability, comp)
    if (is.finite(alpha)) constraints <- rbind(constraints, data.frame(
      constraint_id = sprintf("C%04d", nrow(constraints) + 1L),
      constraint_type = "reliability", target = comp, value = as.character(alpha),
      priority = "soft", tolerance = 0.10, source = "user specification",
      stringsAsFactors = FALSE))
  }
  constraints <- rbind(constraints, data.frame(
    constraint_id = sprintf("C%04d", nrow(constraints) + 1L),
    constraint_type = "within_composite_correlation", target = "default",
    value = as.character(target_correlation), priority = "soft", tolerance = 0.10,
    source = "user specification", stringsAsFactors = FALSE))
  out <- structure(list(
    data = dat, constraints = constraints, validation = validation,
    metadata = list(data_status = "synthetic_analytical_reconstruction",
      historical_respondents_recovered = FALSE, respondent_n = n,
      seed = as.integer(seed), generated_at = lbsr_now(),
      generator = "frequency_rank_copula_v1", target_reliability = target_reliability,
      target_correlation = target_correlation, group_tolerance = group_tolerance)
  ), class = "lbsr_reconstruction")
  out$fidelity <- score_multidimensional_fidelity(out, x, target_reliability)
  out
}

lbsr_fidelity_row <- function(metric, target_id, target, achieved, tolerance) {
  error <- abs(achieved - target)
  data.frame(metric = metric, target_id = target_id, target = target,
             achieved = achieved, absolute_error = error, tolerance = tolerance,
             pass = is.finite(error) & error <= tolerance, stringsAsFactors = FALSE)
}

#' Score multidimensional reconstruction fidelity
#'
#' @param reconstruction An `lbsr_reconstruction`.
#' @param evidence The source evidence bundle.
#' @param target_reliability Scalar or named alpha targets.
#' @return A long-form metric table with a summary attribute.
#' @export
score_multidimensional_fidelity <- function(reconstruction, evidence,
                                             target_reliability = 0.70) {
  if (!inherits(reconstruction, "lbsr_reconstruction"))
    stop("reconstruction must be an lbsr_reconstruction.", call. = FALSE)
  dat <- reconstruction$data; rows <- list(); q <- 0L
  add <- function(...) { q <<- q + 1L; rows[[q]] <<- lbsr_fidelity_row(...) }
  for (i in seq_len(nrow(evidence$items))) {
    code <- as.character(evidence$items$code[[i]]); v <- dat[[code]]
    add("item_mean", code, as.numeric(evidence$items$mean[[i]]), mean(v), 0.10)
    if (is.finite(as.numeric(evidence$items$sd[[i]])))
      add("item_sd", code, as.numeric(evidence$items$sd[[i]]), stats::sd(v), 0.20)
  }
  comps <- lbsr_composite_codes(evidence$items)
  for (comp in unique(comps)) {
    codes <- as.character(evidence$items$code[comps == comp]); mat <- dat[codes]
    probs <- lbsr_frequency_probabilities(evidence$composite_frequencies, comp)
    if (all(is.finite(probs)) && sum(probs) > 0) for (cat in 1:5)
      add("composite_frequency_proportion", paste(comp, cat, sep = "::"), probs[[cat]],
          mean(as.matrix(mat) == cat), 0.05)
    alpha_target <- lbsr_named_target(target_reliability, comp)
    if (ncol(mat) >= 2L && is.finite(as.numeric(alpha_target)))
      add("cronbach_alpha", comp, as.numeric(alpha_target), cronbach_alpha(mat), 0.10)
  }
  if ("group" %in% names(dat) && nrow(evidence$group_means)) {
    total_labels <- c("skupaj", "total", "all")
    gm <- evidence$group_means[
      !tolower(trimws(as.character(evidence$group_means$pv_ownership_group))) %in% total_labels,
      , drop = FALSE]
    for (i in seq_len(nrow(gm))) {
      g <- as.character(gm$pv_ownership_group[[i]])
      codes <- as.character(evidence$items$code[comps == gm$composite_code[[i]]])
      if (g %in% dat$group && length(codes))
        add("group_composite_mean", paste(gm$composite_code[[i]], g, sep = "::"),
            as.numeric(gm$mean[[i]]), mean(as.matrix(dat[dat$group == g, codes, drop = FALSE])),
            0.15)
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else lbsr_fidelity_row(
    character(), character(), numeric(), numeric(), numeric())
  split_m <- split(seq_len(nrow(out)), out$metric)
  summary <- do.call(rbind, lapply(names(split_m), function(m) {
    z <- out[split_m[[m]], , drop = FALSE]
    data.frame(metric = m, n_targets = nrow(z), pass_rate = mean(z$pass),
               mean_absolute_error = mean(z$absolute_error, na.rm = TRUE),
               max_absolute_error = max(z$absolute_error, na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
  attr(out, "summary") <- summary
  out
}
