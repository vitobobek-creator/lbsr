test_that("decision records are appended and hash chained", {
  path <- withr::local_tempdir()
  p <- create_lbsr_project(file.path(path,"case"), "Case", "Owner")
  p <- record_decision(p, "CON-001", "Reviewer", "A | B", "A",
                       "Primary evidence has priority.")
  first <- attr(p, "decision")
  p <- record_decision(p, "CON-002", "Reviewer", "C | D", "D",
                       "Methodological validity requires exclusion.")
  second <- attr(p, "decision")
  expect_equal(first$previous_record_hash, "GENESIS")
  expect_equal(second$previous_record_hash, first$record_hash)
  log <- read.csv(file.path(p$path,"logs","decision-log.csv"), stringsAsFactors=FALSE)
  expect_equal(nrow(log), 2)
  expect_length(p$manifest$decisions, 2)
})

