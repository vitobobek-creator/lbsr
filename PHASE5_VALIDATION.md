# Phase 5 validation record

Version 0.4.0 adds multivariate reconstruction and validation.

## Implemented controls

- reported stacked category frequencies are normalized to proportional targets;
- numeric, English, and canonical Slovenian Likert labels are decoded explicitly,
  including the documented legacy fifth-category wording variant;
- largest-remainder allocation produces integer category counts;
- item totals are calibrated to their reported means within finite-N precision;
- a seed-controlled Gaussian rank copula induces within-composite association
  from the declared correlation assumption;
- Cronbach's alpha targets can be scalar or composite-specific and are evaluated
  as fidelity criteria rather than used as a redundant second association control;
- group calibration uses value swaps, preserving global item marginals;
- group labels must explicitly match the evidence; unmatched groups are not guessed;
- fidelity is separated into item mean, item SD, category proportion,
  group-composite mean, and reliability metric families;
- output retains mandatory synthetic-data and non-recovery provenance.

## Interpretation safeguards

Frequency counts are not treated as respondent counts. Reliability and correlation
targets are declared modelling assumptions because the available documentary evidence
does not identify them. A failed fidelity target remains in the report and is not
relabelled as satisfied.

## Scope boundary

This is a transparent rank-copula calibration engine, not a claim of unique recovery.
It does not estimate the unknowable historical covariance matrix. Formal SEM estimation,
sensitivity ensembles, export profiles, and manuscript-ready validation reports remain
future phases.
