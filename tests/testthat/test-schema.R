test_that("canonical tables pass structural validation", {
  result <- validate_coding_book_schema(make_valid_tables())
  expect_s3_class(result, "lbsr_validation")
  expect_true(result$valid)
})

test_that("missing fields fail structural validation", {
  tables <- make_valid_tables()
  tables$items$code <- NULL
  result <- validate_coding_book_schema(tables)
  expect_false(result$valid)
  expect_true("LBSR-SCHEMA-011" %in% result$findings$code)
})

test_that("semantic validator flags scale and stacked N", {
  tables <- make_valid_tables()
  tables$items$mean <- 6
  bundle <- do.call(lbsr:::new_lbsr_evidence_bundle, tables)
  result <- validate_evidence_bundle(bundle)
  expect_false(result$valid)
  expect_true("LBSR-V001" %in% result$findings$code)
  expect_true("LBSR-N001" %in% result$findings$code)
})

test_that("name normalisation is stable", {
  expect_equal(normalize_lbsr_names(c("P-value", "Internal source location")),
               c("p_value", "internal_source_location"))
})

