\name{create_lbsr_project}
\alias{create_lbsr_project}
\title{Create an LBSR project}
\usage{create_lbsr_project(path, title, owner, project_id = NULL,
description = "", overwrite = FALSE)}
\arguments{
\item{path}{Directory for the new project.}
\item{title}{Project title.}
\item{owner}{Named project owner.}
\item{project_id}{Optional stable project identifier.}
\item{description}{Optional description.}
\item{overwrite}{Whether to replace an existing manifest.}
}
\value{An object of class \code{lbsr_project}.}
\description{Creates the standard project directories and versioned JSON manifest.}

