lbsr_manifest <- function(project) {
  if (inherits(project, "lbsr_project")) return(project$manifest)
  if (is.character(project) && length(project) == 1L) {
    file <- if (dir.exists(project)) file.path(project, "lbsr-manifest.json") else project
    if (!file.exists(file)) stop("Manifest not found: ", file, call. = FALSE)
    return(jsonlite::fromJSON(file, simplifyVector = FALSE))
  }
  stop("project must be an lbsr_project, project directory, or manifest path.", call. = FALSE)
}

write_lbsr_manifest <- function(project) {
  stopifnot(inherits(project, "lbsr_project"))
  project$manifest$updated_at <- lbsr_now()
  text <- jsonlite::toJSON(project$manifest, auto_unbox = TRUE, pretty = TRUE,
                          null = "null", na = "null")
  writeLines(text, file.path(project$path, "lbsr-manifest.json"), useBytes = TRUE)
  project
}

create_lbsr_project <- function(path, title, owner, project_id = NULL,
                                description = "", overwrite = FALSE) {
  lbsr_scalar_character(path, "path")
  lbsr_scalar_character(title, "title")
  lbsr_scalar_character(owner, "owner")
  lbsr_scalar_character(description, "description", allow_empty = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  manifest_file <- file.path(path, "lbsr-manifest.json")
  if (file.exists(manifest_file) && !isTRUE(overwrite)) {
    stop("An LBSR project already exists at path. Set overwrite = TRUE only to replace the manifest.", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for (d in .lbsr_directories) dir.create(file.path(path, d), showWarnings = FALSE)
  project_id <- project_id %||% paste0(
    "LBSR-", format(Sys.Date(), "%Y%m%d"), "-",
    toupper(substr(digest::digest(paste(path, title, owner), algo = "sha256"), 1L, 8L))
  )
  created <- lbsr_now()
  manifest <- list(
    manifest_version = .lbsr_manifest_version,
    project_id = project_id,
    title = title,
    description = description,
    owner = owner,
    status = "CREATED",
    data_status = "not_yet_classified",
    package_phase = .lbsr_package_phase,
    created_at = created,
    updated_at = created,
    directories = as.list(setNames(.lbsr_directories, .lbsr_directories)),
    sources = list(),
    decisions = list(),
    software = list(package = "lbsr", version = "0.2.0", r_version = R.version.string)
  )
  project <- new_lbsr_project(path, manifest)
  project <- write_lbsr_manifest(project)
  notice <- paste(
    "LBSR project", project_id,
    "Synthetic analytical reconstructions are not recovered historical respondents."
  )
  writeLines(notice, file.path(path, "README.txt"), useBytes = TRUE)
  project
}

validate_lbsr_manifest <- function(x) {
  manifest <- if (inherits(x, "lbsr_project") || is.character(x)) lbsr_manifest(x) else x
  required <- c("manifest_version", "project_id", "title", "owner", "status",
                "data_status", "created_at", "updated_at", "directories", "sources")
  findings <- lbsr_findings()
  missing <- setdiff(required, names(manifest))
  for (field in missing) {
    findings <- add_finding(findings, "LBSR-SCHEMA-001", "ERROR", "manifest",
                            field, NA_integer_, paste("Missing manifest field:", field))
  }
  if (!length(missing) && !identical(as.character(manifest$manifest_version), .lbsr_manifest_version)) {
    findings <- add_finding(findings, "LBSR-SCHEMA-002", "WARNING", "manifest",
                            "manifest_version", NA_integer_, "Manifest version differs from the package baseline.")
  }
  new_lbsr_validation(findings)
}
