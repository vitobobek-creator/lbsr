validate_coding_book_schema <- function(x) {
  required_names <- names(lbsr_schema())
  findings <- lbsr_findings()
  if (inherits(x, "lbsr_evidence_bundle")) {
    tables <- unclass(x)[required_names]
  } else if (is.list(x)) {
    tables <- x
  } else {
    stop("x must be an evidence bundle or named list of coding-book tables.", call. = FALSE)
  }
  missing_tables <- setdiff(required_names, names(tables))
  for (tab in missing_tables) findings <- add_finding(
    findings, "LBSR-SCHEMA-020", "ERROR", tab,
    message = paste("Missing coding-book table:", tab)
  )
  for (tab in intersect(required_names, names(tables))) {
    findings <- rbind(findings, validate_table_schema(tables[[tab]], tab))
  }
  new_lbsr_validation(findings)
}

validate_evidence_bundle <- function(x, scale_min = 1, scale_max = 5,
                                     expected_items = NULL,
                                     expected_composites = NULL) {
  if (!inherits(x, "lbsr_evidence_bundle")) stop("x must be an lbsr_evidence_bundle.", call. = FALSE)
  base <- validate_coding_book_schema(x)
  findings <- base$findings
  if (!base$valid) return(new_lbsr_validation(findings))
  items <- x$items
  numeric_fields <- c("item_no", "n", "mean", "sd")
  for (field in numeric_fields) items[[field]] <- suppressWarnings(as.numeric(items[[field]]))
  invalid_mean <- which(!is.na(items$mean) & (items$mean < scale_min | items$mean > scale_max))
  for (i in invalid_mean) findings <- add_finding(findings, "LBSR-V001", "ERROR",
    "items", "mean", i, "Item mean falls outside the declared scale bounds.")
  invalid_n <- which(!is.na(items$n) & (items$n <= 0 | items$n != floor(items$n)))
  for (i in invalid_n) findings <- add_finding(findings, "LBSR-N003", "ERROR",
    "items", "n", i, "Item N must be a positive integer.")
  invalid_sd <- which(!is.na(items$sd) & items$sd < 0)
  for (i in invalid_sd) findings <- add_finding(findings, "LBSR-V003", "ERROR",
    "items", "sd", i, "Standard deviation must be non-negative.")
  if (!is.null(expected_items) && nrow(items) != expected_items) findings <- add_finding(
    findings, "LBSR-M003", "MAJOR", "items", row = NA_integer_,
    message = paste("Expected", expected_items, "items but imported", nrow(items), ".")
  )
  item_composites <- unique(paste(items$tpb_block_code, items$dimension_code, sep = "_"))
  if (!is.null(expected_composites) && length(item_composites) != expected_composites) {
    findings <- add_finding(findings, "LBSR-M004", "MAJOR", "items",
      message = paste("Expected", expected_composites,
                      "construct-dimension composites but inferred", length(item_composites), "."))
  }
  group <- x$group_means
  group$n <- suppressWarnings(as.numeric(group$n))
  stacked_candidates <- which(!is.na(group$n) & group$n > max(items$n, na.rm = TRUE))
  for (i in stacked_candidates) findings <- add_finding(
    findings, "LBSR-N001", "WARNING", "group_means", "n", i,
    "Reported N exceeds item-level respondent N and may represent stacked item responses."
  )
  tests <- x$hypothesis_tests
  tests$p_value <- suppressWarnings(as.numeric(tests$p_value))
  bad_p <- which(!is.na(tests$p_value) & (tests$p_value < 0 | tests$p_value > 1))
  for (i in bad_p) findings <- add_finding(findings, "LBSR-V004", "ERROR",
    "hypothesis_tests", "p_value", i, "p-value must be between 0 and 1.")
  new_lbsr_validation(findings)
}

