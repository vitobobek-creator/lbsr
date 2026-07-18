# lbsr

`lbsr` is the computational foundation of the Legacy Behavioural Survey
Reconstruction (LBSR) Toolkit. Version 0.5.0 implements Phases 2--6 of the
LBSR Software Requirements and Validation Specification:

- canonical S3 data objects for projects, evidence bundles and validation;
- a versioned JSON project manifest;
- source registration with SHA-256 checksums;
- import of the LBSR Excel master coding book;
- schema and semantic validation;
- structured errors, warnings and informational findings;
- respondent-versus-stacked-observation diagnostics;
- item-multiplicity and group-count inference;
- pseudoreplication warnings;
- duplicate statistical-pattern and source-conflict detection;
- evidence-status recommendations and append-only decisions;
- hard, soft, and diagnostic reconstruction constraints;
- infeasible-constraint detection;
- seed-controlled synthetic Likert reconstruction;
- per-target fidelity scoring with mandatory synthetic-data provenance;
- frequency-aware multivariate generation using a rank copula;
- optional group-mean calibration and reliability targets;
- multidimensional fidelity summaries;
- deterministic reconstruction ensembles across seeds and assumptions;
- target stability and parameter-effect summaries;
- transparent preferred-run selection and manuscript-ready sensitivity reports;
- reproducible test fixtures and automated tests.

This version generates baseline and frequency-aware multivariate synthetic
reconstructions. It does **not** claim unique recovery or estimate SEM models.

## Installation

```r
# install.packages("remotes")
remotes::install_local("path/to/lbsr")
```

Required runtime packages are `digest`, `jsonlite`, and `readxl`. Development
tests use `testthat`.

## Quick start

```r
library(lbsr)

project <- create_lbsr_project(
  path = "my_lbsr_project",
  title = "Legacy REC survey reconstruction",
  owner = "Research team"
)

project <- register_sources(
  project,
  files = "REC_reconstruction_master_coding_book.xlsx",
  source_type = "coding_book",
  authority = "derived_primary_input"
)

bundle <- import_coding_book(
  "REC_reconstruction_master_coding_book.xlsx",
  project = project
)

print(bundle)
validation_summary(bundle$validation)

diagnostics <- run_lbsr_diagnostics(
  bundle,
  respondent_n = 66,
  claimed_item_counts = c(28, 62)
)
print(diagnostics)

reconstruction <- reconstruct_synthetic(
  bundle,
  respondent_n = 66,
  seed = 2026,
  group_sizes = c(non_owner = 58, owner = 8)
)
print(reconstruction)
attr(reconstruction$fidelity, "summary")

multivariate <- reconstruct_multivariate(
  bundle,
  respondent_n = 66,
  seed = 2026,
  group_sizes = c(NE = 58, DA = 8),
  target_reliability = 0.70
)
attr(multivariate$fidelity, "summary")

ensemble <- reconstruct_ensemble(
  bundle,
  respondent_n = 66,
  seeds = 1:5,
  reliability_values = c(0.60, 0.70, 0.80),
  correlation_values = c(0.15, 0.25, 0.35),
  group_sizes = c(NE = 58, DA = 8)
)
print(ensemble)
write_sensitivity_report(ensemble, "sensitivity-report.md")
```

## Data-status safeguard

The package distinguishes original data, documentary evidence, derived
evidence, and synthetic analytical reconstruction. Synthetic records must not
be described as recovered respondents.

## Current scope and roadmap

Version 0.5.0 covers ingestion, diagnostics, conflict detection, decisions,
baseline and multivariate reconstruction, multidimensional fidelity, and
sensitivity ensembles. Planned phases add SEM-safe exports, publication reports,
and an optional Shiny interface.
