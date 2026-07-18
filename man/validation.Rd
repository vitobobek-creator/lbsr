\name{validate_coding_book_schema}
\alias{validate_coding_book_schema}
\alias{validate_evidence_bundle}
\alias{validate_lbsr_manifest}
\alias{validation_summary}
\title{Validate LBSR structures}
\description{Runs structural and semantic validation and returns structured findings.}
\usage{
validate_coding_book_schema(x)
validate_evidence_bundle(x, scale_min = 1, scale_max = 5,
expected_items = NULL, expected_composites = NULL)
validate_lbsr_manifest(x)
validation_summary(x)
}
\arguments{
\item{x}{An LBSR object or compatible structure.}
\item{scale_min, scale_max}{Declared response-scale bounds.}
\item{expected_items}{Optional expected item count.}
\item{expected_composites}{Optional expected composite count.}
}
\value{An \code{lbsr_validation} object or its summary.}

