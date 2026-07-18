\name{register_sources}
\alias{register_sources}
\title{Register evidence sources}
\usage{register_sources(project, files, source_type, authority,
source_ids = NULL, titles = NULL, notes = NULL, copy_into_project = FALSE)}
\arguments{
\item{project}{An \code{lbsr_project}.}
\item{files}{Existing source file paths.}
\item{source_type}{Source type labels.}
\item{authority}{Evidence authority labels.}
\item{source_ids}{Optional stable IDs.}
\item{titles}{Optional source titles.}
\item{notes}{Optional notes.}
\item{copy_into_project}{Copy sources into the project sources directory.}
}
\value{Updated \code{lbsr_project}.}
\description{Registers checksummed source files in the project manifest.}

