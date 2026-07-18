\name{multivariate_reconstruction}
\alias{reconstruct_multivariate}
\alias{score_multidimensional_fidelity}
\alias{cronbach_alpha}
\title{Multivariate synthetic reconstruction and fidelity scoring}
\usage{
reconstruct_multivariate(x, respondent_n, seed = 2026L, group_sizes = NULL,
  target_reliability = 0.70, target_correlation = 0.25,
  group_tolerance = 0.15, max_swaps = 10000L)

score_multidimensional_fidelity(reconstruction, evidence,
  target_reliability = 0.70)

cronbach_alpha(x)
}
\arguments{
  \item{x}{For reconstruction, an \code{lbsr_evidence_bundle}; for alpha, a numeric item matrix.}
  \item{respondent_n}{Number of synthetic records.}
  \item{seed}{Integer random seed.}
  \item{group_sizes}{Optional named group sizes matching evidence labels.}
  \item{target_reliability}{Scalar or named vector of Cronbach alpha evaluation targets.}
  \item{target_correlation}{Within-composite rank-correlation generation assumption.}
  \item{group_tolerance}{Absolute group-mean tolerance.}
  \item{max_swaps}{Maximum calibration swaps per target.}
  \item{reconstruction}{An \code{lbsr_reconstruction}.}
  \item{evidence}{The source evidence bundle.}
}
\value{A reconstruction object, long-form fidelity table, or alpha coefficient.}
\description{Generates frequency-aware, correlated synthetic Likert records, optionally
calibrates group means, estimates reliability, and reports fidelity by metric family.
The output is synthetic analytical reconstruction, not recovered historical data.}
