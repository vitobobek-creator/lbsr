\name{reconstruction}
\alias{classify_constraints}
\alias{validate_constraints}
\alias{reconstruct_synthetic}
\alias{score_fidelity}
\title{Classify constraints and create a synthetic reconstruction}
\usage{
classify_constraints(x, respondent_n, group_sizes = NULL)

validate_constraints(constraints)

reconstruct_synthetic(x, respondent_n, seed = 2026L, group_sizes = NULL)

score_fidelity(reconstruction, evidence)
}
\arguments{
  \item{x}{An \code{lbsr_evidence_bundle}.}
  \item{respondent_n}{Number of synthetic respondents.}
  \item{group_sizes}{Optional named vector of hard group sizes.}
  \item{constraints}{A constraint register.}
  \item{seed}{Integer seed.}
  \item{reconstruction}{An \code{lbsr_reconstruction}.}
  \item{evidence}{The source evidence bundle.}
}
\value{A constraint data frame, validation result, reconstruction object, or fidelity table.}
\description{Creates auditable reconstruction constraints, rejects infeasible hard constraints,
generates seed-controlled integer Likert records, and scores statistical fidelity. Generated
records are explicitly synthetic and are not recovered historical respondents.}
