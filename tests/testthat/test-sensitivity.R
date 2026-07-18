test_that("ensemble spans assumptions and produces stability summaries", {
  x <- make_multivariate_bundle()
  e <- reconstruct_ensemble(x, 30, seeds = 1:2, reliability_values = c(.6, .8),
                            correlation_values = c(.1, .4), max_runs = 8)
  expect_s3_class(e, "lbsr_sensitivity")
  expect_equal(nrow(e$registry), 8)
  expect_true(all(e$registry$status == "succeeded"))
  expect_true(nrow(e$summary$target_stability) > 0)
  expect_true(all(e$summary$target_stability$classification %in%
                    c("stable_pass", "stable_fail", "assumption_sensitive")))
  expect_identical(e$metadata$comparison_unit,
                   "aggregate_targets_not_synthetic_rows")
})

test_that("ensemble and preferred-run selection are deterministic", {
  x <- make_multivariate_bundle()
  args <- list(x = x, respondent_n = 30, seeds = 1:2,
               reliability_values = .7, correlation_values = c(.1, .3))
  a <- do.call(reconstruct_ensemble, args); b <- do.call(reconstruct_ensemble, args)
  expect_identical(a$registry, b$registry)
  expect_identical(a$fidelity, b$fidelity)
  selected <- select_sensitivity_run(a)
  expect_s3_class(selected, "lbsr_reconstruction")
  expect_identical(selected$metadata$selection_rule, "min_normalized_loss")
  expect_false(selected$metadata$selection_is_methodological_truth)
})

test_that("report records safeguards and grid limit is enforced", {
  x <- make_multivariate_bundle()
  expect_error(reconstruct_ensemble(x, 30, seeds = 1:3,
    reliability_values = c(.6, .7), correlation_values = c(.1, .2), max_runs = 5),
    "exceeds max_runs")
  e <- reconstruct_ensemble(x, 30, seeds = 1:2,
                            reliability_values = .7, correlation_values = .2)
  path <- tempfile(fileext = ".md")
  write_sensitivity_report(e, path)
  report <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(report, "does not recover historical respondents", fixed = TRUE)
  expect_match(report, "Synthetic rows are not matched across runs", fixed = TRUE)
})
