# Phase 4 validation record

Version 0.3.0 introduces an auditable minimum reconstruction engine.

## Implemented controls

- constraints are classified as hard, soft, or diagnostic;
- malformed, out-of-range, and inconsistent hard constraints are rejected;
- group sizes must sum exactly to the requested respondent count;
- Likert records are integers bounded to 1--5;
- item means are matched to the closest attainable total at the requested N;
- generation is repeatable for a documented seed and restores caller RNG state;
- every output states that it is a synthetic analytical reconstruction and that
  historical respondent records were not recovered;
- fidelity is reported per target with pass/fail, absolute error, and summary.

## Scope boundary

This engine provides a transparent baseline reconstruction for validation and
software testing. It does not yet optimize correlations, group-specific means,
frequency tables, reliability, or SEM fit. Composite means and legacy tests are
retained in the constraint register, but the former are not independently fitted
when they conflict with item means and the latter are diagnostic only.

## Verification status

Source and fixture checks were completed statically in the development workspace.
The included testthat suite and GitHub Actions workflow are the authoritative
runtime checks once an R environment executes the package.
