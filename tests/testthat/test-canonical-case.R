test_that("canonical case distinguishes respondents from stacked counts", {
  respondents <- 66
  item_count <- 3
  expect_equal(respondents * item_count, 198)
  expect_equal(174 / item_count, 58)
  expect_equal(24 / item_count, 8)
})

test_that("external canonical workbook can be imported", {
  file <- Sys.getenv("LBSR_CANONICAL_CODING_BOOK", unset = "")
  skip_if(!nzchar(file) || !file.exists(file),
          "Set LBSR_CANONICAL_CODING_BOOK to run the private acceptance fixture.")
  bundle <- import_coding_book(file)
  expect_equal(nrow(bundle$items), 62)
  expect_equal(length(unique(paste(bundle$items$tpb_block_code,
                                   bundle$items$dimension_code, sep = "_"))), 16)
  diagnostics <- run_lbsr_diagnostics(
    bundle, respondent_n = 66, claimed_item_counts = c(28, 62)
  )
  expect_true(any(diagnostics$sample_sizes$count_type == "stacked_item_responses"))
  expect_true(any(diagnostics$conflicts$conflict_type == "inventory_item_count"))
  expect_true("LBSR-B001" %in% diagnostics$issues$code)
})
