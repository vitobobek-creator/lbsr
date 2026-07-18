\name{diagnose_evidence}
\alias{diagnose_evidence}
\alias{run_lbsr_diagnostics}
\alias{diagnose_sample_sizes}
\alias{detect_source_conflicts}
\title{Diagnose sample sizes and evidence conflicts}
\usage{
diagnose_sample_sizes(x, respondent_n = NULL,
total_group_labels = c("skupaj", "total", "all"))
detect_source_conflicts(x, mean_tolerance = 0.10,
duplicate_digits = 3L, claimed_item_counts = NULL)
diagnose_evidence(x, respondent_n = NULL,
claimed_item_counts = NULL, mean_tolerance = 0.10)
run_lbsr_diagnostics(x, respondent_n = NULL,
claimed_item_counts = NULL, mean_tolerance = 0.10)
}
\arguments{
\item{x}{An \code{lbsr_evidence_bundle}.}
\item{respondent_n}{Optional confirmed respondent count.}
\item{total_group_labels}{Labels identifying total-group rows.}
\item{mean_tolerance}{Maximum compatible absolute mean difference.}
\item{duplicate_digits}{Rounding precision used for duplicate signatures.}
\item{claimed_item_counts}{Optional item-count claims from external documentation.}
}
\value{An \code{lbsr_diagnostics} object.}
\description{Separates respondent counts from stacked item responses, infers
item multiplicity and group respondent counts, warns about pseudoreplication,
and detects statistical or inventory conflicts.}

