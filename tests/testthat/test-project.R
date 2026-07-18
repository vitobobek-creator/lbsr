test_that("project creation writes a valid manifest", {
  path <- withr::local_tempdir()
  project <- create_lbsr_project(file.path(path, "case"), "Test project", "Test owner")
  expect_s3_class(project, "lbsr_project")
  expect_true(file.exists(file.path(project$path, "lbsr-manifest.json")))
  expect_true(validate_lbsr_manifest(project)$valid)
  expect_true(all(file.exists(file.path(project$path,
    c("sources", "config", "derived", "reports", "exports", "logs")))))
})

test_that("source registration stores checksum and updates state", {
  path <- withr::local_tempdir()
  source <- file.path(path, "source.txt")
  writeLines("evidence", source)
  project <- create_lbsr_project(file.path(path, "case"), "Test", "Owner")
  project <- register_sources(project, source, "documentation", "primary")
  expect_equal(project$manifest$status, "SOURCES_REGISTERED")
  expect_length(project$manifest$sources, 1)
  expect_match(project$manifest$sources[[1]]$checksum_sha256, "^[a-f0-9]{64}$")
})

