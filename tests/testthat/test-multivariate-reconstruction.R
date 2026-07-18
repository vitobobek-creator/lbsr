test_that("multivariate reconstruction is deterministic and auditable", {
  x <- make_multivariate_bundle()
  a <- reconstruct_multivariate(x, 66, seed = 7, target_reliability = 0.70)
  b <- reconstruct_multivariate(x, 66, seed = 7, target_reliability = 0.70)
  expect_identical(a$data, b$data)
  expect_true(all(as.matrix(a$data[c("ATT_TEC1", "ATT_TEC2")]) %in% 1:5))
  expect_equal(mean(a$data$ATT_TEC1), 3.55, tolerance = 0.01)
  expect_equal(mean(a$data$ATT_TEC2), 3.25, tolerance = 0.01)
  expect_true(any(a$constraints$constraint_type == "reliability"))
  expect_identical(a$metadata$generator, "frequency_rank_copula_v1")
})

test_that("multidimensional fidelity reports separate metric families", {
  x <- make_multivariate_bundle()
  a <- reconstruct_multivariate(x, 66, seed = 8)
  expect_true(all(c("item_mean", "item_sd", "composite_frequency_proportion",
                    "cronbach_alpha") %in% unique(a$fidelity$metric)))
  s <- attr(a$fidelity, "summary")
  expect_true(all(c("pass_rate", "mean_absolute_error", "max_absolute_error") %in% names(s)))
})

test_that("Cronbach alpha handles ordinary and degenerate inputs", {
  expect_true(is.finite(cronbach_alpha(data.frame(a = 1:5, b = 1:5))))
  expect_true(is.na(cronbach_alpha(data.frame(a = 1:5))))
})
