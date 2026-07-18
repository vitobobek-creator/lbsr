lbsr_decision_fields <- c(
  "decision_id", "issue_id", "created_at", "actor", "alternatives",
  "decision", "rationale", "evidence_ids", "impact", "approval_status",
  "supersedes", "software_version", "record_hash", "previous_record_hash"
)

lbsr_decision_file <- function(project) file.path(project$path, "logs", "decision-log.csv")

read_decision_log <- function(project) {
  file <- lbsr_decision_file(project)
  if (!file.exists(file)) return(lbsr_empty_df(lbsr_decision_fields))
  utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE,
                  colClasses = "character")
}

record_decision <- function(project, issue_id, actor, alternatives, decision,
                            rationale, evidence_ids = "", impact = "",
                            approval_status = "approved", supersedes = "") {
  if (!inherits(project, "lbsr_project")) {
    stop("project must be an lbsr_project.", call. = FALSE)
  }
  values <- list(issue_id = issue_id, actor = actor, alternatives = alternatives,
                 decision = decision, rationale = rationale,
                 approval_status = approval_status)
  for (name in names(values)) lbsr_scalar_character(values[[name]], name)
  allowed <- c("proposed", "approved", "rejected", "superseded")
  if (!approval_status %in% allowed) {
    stop("approval_status must be one of: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  log <- read_decision_log(project)
  previous_hash <- if (nrow(log)) log$record_hash[[nrow(log)]] else "GENESIS"
  decision_id <- paste0("DEC-", format(Sys.Date(), "%Y%m%d"), "-",
                        sprintf("%04d", nrow(log) + 1L))
  created_at <- lbsr_now()
  payload <- paste(decision_id, issue_id, created_at, actor, alternatives,
                   decision, rationale, evidence_ids, impact, approval_status,
                   supersedes, previous_hash, sep = "|")
  record_hash <- digest::digest(payload, algo = "sha256")
  row <- data.frame(
    decision_id = decision_id, issue_id = issue_id, created_at = created_at,
    actor = actor, alternatives = alternatives, decision = decision,
    rationale = rationale, evidence_ids = evidence_ids, impact = impact,
    approval_status = approval_status, supersedes = supersedes,
    software_version = "0.2.0", record_hash = record_hash,
    previous_record_hash = previous_hash, stringsAsFactors = FALSE
  )
  log <- rbind(log, row)
  utils::write.csv(log, lbsr_decision_file(project), row.names = FALSE,
                   fileEncoding = "UTF-8", na = "")
  project$manifest$decisions <- c(project$manifest$decisions %||% list(),
    list(list(decision_id = decision_id, issue_id = issue_id,
              approval_status = approval_status, record_hash = record_hash,
              created_at = created_at)))
  project <- write_lbsr_manifest(project)
  attr(project, "decision") <- row
  project
}

