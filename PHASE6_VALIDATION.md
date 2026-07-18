# Phase 6 validation record

Version 0.5.0 introduces sensitivity ensembles and uncertainty reporting.

## Implemented controls

- deterministic Cartesian grids over seed, reliability, and correlation assumptions;
- a configurable maximum-run guard against accidental combinatorial expansion;
- failed runs are retained in the registry with their error messages;
- run ranking uses tolerance-normalized loss averaged equally across metric families,
  preventing a family with more documentary targets from dominating by row count;
  ties are broken deterministically;
- preferred-run selection records its rule and explicitly denies methodological truth;
- target-level summaries report achieved mean, SD, range, and pass rate;
- targets are classified as stable pass, stable fail, or assumption-sensitive;
- parameter-effect summaries separate reliability and correlation assumptions;
- manuscript-ready Markdown reports include provenance and interpretation safeguards;
- synthetic respondents are never paired across runs; only aggregate targets are compared.

## Interpretation

The ensemble estimates sensitivity to declared reconstruction choices, not sampling
uncertainty from the original survey. A preferred run is a convenient representative
under an explicit loss function and is not evidence of unique historical recovery.

## Scope boundary

This phase does not supply inferential confidence intervals for the historical population.
Bootstrap or multiple-imputation terminology must not be used unless a future method
establishes the assumptions required for those interpretations.
