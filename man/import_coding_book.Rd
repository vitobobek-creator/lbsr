\name{import_coding_book}
\alias{import_coding_book}
\title{Import an LBSR master coding book}
\usage{import_coding_book(file, project = NULL, strict = TRUE,
expected_items = 62L, expected_composites = 16L)}
\arguments{
\item{file}{Path to an Excel master coding book.}
\item{project}{Optional \code{lbsr_project}.}
\item{strict}{Stop when structural schema errors are detected.}
\item{expected_items}{Expected item count, or NULL.}
\item{expected_composites}{Expected cross-classified composite count, or NULL.}
}
\value{An \code{lbsr_evidence_bundle}.}
\description{Imports, normalises, and validates the five canonical coding-book sheets.}

