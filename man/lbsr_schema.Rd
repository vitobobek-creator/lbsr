\name{lbsr_schema}
\alias{lbsr_schema}
\alias{normalize_lbsr_names}
\alias{lbsr_manifest}
\title{Inspect LBSR schemas and manifests}
\usage{
lbsr_schema(name = NULL)
normalize_lbsr_names(x)
lbsr_manifest(project)
}
\arguments{
\item{name}{Optional schema name.}
\item{x}{Names to normalise.}
\item{project}{Project object, directory, or manifest path.}
}
\value{A schema, normalised names, or project manifest.}
\description{Utility accessors for the LBSR package data contract.}

