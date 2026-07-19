test_that("constraints are classified and infeasibility is detected", {
  x <- do.call(new_lbsr_evidence_bundle, make_valid_tables())
  con <- classify_constraints(x, 66, group_sizes = c(non_owner = 58, owner = 8))
  expect_true(all(c("hard", "soft", "diagnostic") %in% con$priority))
  expect_true(validate_constraints(con)$valid)
  con$value[con$constraint_type == "group_size" & con$target == "owner"] <- "9"
  expect_false(validate_constraints(con)$valid)
})

test_that("reconstruction is reproducible, bounded, and labelled synthetic", {
  x <- do.call(new_lbsr_evidence_bundle, make_valid_tables())
  a <- reconstruct_synthetic(x, 66, seed = 42, group_sizes = c(non_owner = 58, owner = 8))
  b <- reconstruct_synthetic(x, 66, seed = 42, group_sizes = c(non_owner = 58, owner = 8))
  expect_identical(a$data, b$data)
  expect_equal(as.integer(table(a$data$group)), c(58L, 8L))
  expect_true(all(a$data$ATT_TEC1 %in% 1:5))
  expect_identical(a$metadata$data_status, "synthetic_analytical_reconstruction")
  expect_false(a$metadata$historical_respondents_recovered)
  expect_true(all(a$fidelity$pass))
})

test_that("reconstruction does not alter caller RNG state", {
  x <- do.call(new_lbsr_evidence_bundle, make_valid_tables())
  set.seed(99); before <- .Random.seed
  reconstruct_synthetic(x, 10, seed = 1)
  expect_identical(.Random.seed, before)
})
