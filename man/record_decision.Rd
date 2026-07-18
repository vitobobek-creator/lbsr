\name{record_decision}
\alias{record_decision}
\title{Append a methodological decision}
\usage{record_decision(project, issue_id, actor, alternatives, decision,
rationale, evidence_ids = "", impact = "", approval_status = "approved",
supersedes = "")}
\arguments{
\item{project}{An \code{lbsr_project}.}
\item{issue_id}{The conflict or issue being decided.}
\item{actor}{Named decision maker.}
\item{alternatives}{Material alternatives considered.}
\item{decision}{Selected treatment.}
\item{rationale}{Methodological and evidential justification.}
\item{evidence_ids}{Supporting evidence identifiers.}
\item{impact}{Affected mappings, constraints, data, or readiness.}
\item{approval_status}{Proposed, approved, rejected, or superseded.}
\item{supersedes}{Earlier decision ID, if applicable.}
}
\value{The updated project with the appended decision as an attribute.}
\description{Adds a hash-chained record to the project decision log. Existing
records are retained; corrections must be appended and may identify the record
they supersede.}

