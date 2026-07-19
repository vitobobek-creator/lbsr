lbsr_item_multiplicity <- function(items) {
  code <- paste(items$tpb_block_code, items$dimension_code, sep = "_")
  out <- as.data.frame(table(code), stringsAsFactors = FALSE)
  names(out) <- c("composite_code", "item_multiplicity")
  out$item_multiplicity <- as.integer(out$item_multiplicity)
  out
}

lbsr_respondent_n <- function(items, respondent_n = NULL) {
  if (!is.null(respondent_n)) {
    if (length(respondent_n) != 1L || is.na(respondent_n) || respondent_n <= 0) {
      stop("respondent_n must be one positive number.", call. = FALSE)
    }
    return(as.numeric(respondent_n))
  }
  n <- suppressWarnings(as.numeric(items$n))
  n <- n[is.finite(n) & n > 0]
  if (!length(n)) return(NA_real_)
  tab <- sort(table(n), decreasing = TRUE)
  as.numeric(names(tab)[1])
}

diagnose_sample_sizes <- function(x, respondent_n = NULL,
                                  total_group_labels = c("skupaj", "total", "all")) {
  if (!inherits(x, "lbsr_evidence_bundle")) {
    stop("x must be an lbsr_evidence_bundle.", call. = FALSE)
  }
  items <- x$items
  group <- x$group_means
  multiplicity <- lbsr_item_multiplicity(items)
  respondent_n <- lbsr_respondent_n(items, respondent_n)
  group$composite_code <- as.character(group$composite_code)
  group$n <- suppressWarnings(as.numeric(group$n))
  group$pv_ownership_group <- as.character(group$pv_ownership_group)
  merged <- merge(group, multiplicity, by = "composite_code", all.x = TRUE, sort = FALSE)
  rows <- vector("list", nrow(merged))
  issues <- lbsr_findings()
  for (i in seq_len(nrow(merged))) {
    n <- merged$n[[i]]
    k <- merged$item_multiplicity[[i]]
    inferred <- if (is.finite(n) && is.finite(k) && k > 0 && n %% k == 0) n / k else NA_real_
    is_total <- tolower(trimws(merged$pv_ownership_group[[i]])) %in% total_group_labels
    if (!is.finite(n) || !is.finite(k)) {
      type <- "unresolved"; confidence <- "low"
    } else if (k > 1 && !is.na(inferred) && ((!is_total && inferred < respondent_n) ||
                                             (is_total && inferred == respondent_n))) {
      type <- "stacked_item_responses"; confidence <- "high"
      issues <- add_finding(issues, "LBSR-N001", "MAJOR", "group_means", "n", i,
        paste0("Reported N=", n, " equals ", inferred, " respondents x ", k,
               " items for ", merged$composite_code[[i]], "."))
    } else if (n == respondent_n) {
      type <- "respondents"; confidence <- "high"
    } else if (!is.na(inferred) && k > 1) {
      type <- "possible_stacked_item_responses"; confidence <- "medium"
      issues <- add_finding(issues, "LBSR-N004", "WARNING", "group_means", "n", i,
        "Reported N is divisible by item multiplicity but does not reconcile fully with respondent N.")
    } else {
      type <- "unresolved"; confidence <- "low"
      issues <- add_finding(issues, "LBSR-N002", "MAJOR", "group_means", "n", i,
        "Reported N cannot be reconciled with respondent N and item multiplicity.")
    }
    rows[[i]] <- data.frame(
      composite_code = merged$composite_code[[i]],
      group = merged$pv_ownership_group[[i]], reported_n = n,
      item_multiplicity = as.integer(k), inferred_respondents = inferred,
      count_type = type, confidence = confidence, stringsAsFactors = FALSE
    )
  }
  out <- if (length(rows)) do.call(rbind, rows) else new_lbsr_diagnostics()$sample_sizes
  tests <- x$hypothesis_tests
  stacked_codes <- unique(out$composite_code[out$count_type %in%
    c("stacked_item_responses", "possible_stacked_item_responses")])
  affected <- which(as.character(tests$composite_code) %in% stacked_codes)
  for (i in affected) issues <- add_finding(
    issues, "LBSR-B001", "MAJOR", "hypothesis_tests", "test", i,
    paste0("Legacy ", tests$test[[i]], " for ", tests$composite_code[[i]],
           " is linked to stacked item responses; independence must not be assumed."))
  new_lbsr_diagnostics(
    sample_sizes = out, issues = issues,
    metadata = list(respondent_n = respondent_n, generated_at = lbsr_now())
  )
}

