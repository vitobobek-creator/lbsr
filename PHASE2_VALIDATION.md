# Phase 2 verification record

Date: 2026-07-18  
Package version: 0.1.0  
Specification phase: data model, manifest, coding-book import, schemas

## Completed static verification

- All ten exported functions resolve to R function definitions.
- All R source files passed delimiter-balance inspection outside strings and
  comments.
- `DESCRIPTION` passed DCF structural inspection and includes all mandatory
  identity fields.
- Both JSON schema files parsed successfully.
- The GitHub Actions workflow parsed successfully as YAML.
- All five required worksheet names exist in the canonical master coding book.
- Normalised workbook headers match the package schemas exactly:
  - Master Coding Book: 14 of 14 fields;
  - Composite Frequencies: 12 of 12 fields;
  - Group Means: 11 of 11 fields;
  - Hypothesis Tests: 10 of 10 fields;
  - Reconstruction Plan: 4 of 4 fields.
- The private acceptance test expects 62 item records and 16 unique
  TPB-block-by-motivational-dimension composites.
- The regression fixtures preserve the distinction between 66 respondents and
  stacked item-response totals such as 198.

## Runtime verification status

`R`, `Rscript`, and an R package-check environment were not available in the
development workspace. Consequently, `testthat` and `R CMD check` were not
executed here. The repository includes:

- automated unit and regression tests;
- a canonical private-fixture acceptance test;
- `tools/check-canonical-case.R` for an end-to-end workbook import;
- a cross-platform GitHub Actions `R CMD check` workflow.

## Commands for the first R-enabled verification

```r
install.packages(c(
  "digest", "jsonlite", "readxl", "testthat", "withr",
  "knitr", "rmarkdown", "devtools"
))

Sys.setenv(
  LBSR_CANONICAL_CODING_BOOK =
    "path/to/REC_reconstruction_master_coding_book.xlsx"
)

devtools::document("lbsr")
devtools::test("lbsr")
devtools::check("lbsr")
```

End-to-end acceptance import:

```sh
R CMD INSTALL lbsr
Rscript lbsr/tools/check-canonical-case.R \
  path/to/REC_reconstruction_master_coding_book.xlsx
```

