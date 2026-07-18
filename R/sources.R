register_sources <- function(project, files, source_type, authority,
                             source_ids = NULL, titles = NULL, notes = NULL,
                             copy_into_project = FALSE) {
  if (!inherits(project, "lbsr_project")) stop("project must be an lbsr_project.", call. = FALSE)
  files <- as.character(files)
  if (!length(files) || any(!file.exists(files))) stop("Every source file must exist.", call. = FALSE)
  n <- length(files)
  recycle <- function(x, label, default = "") {
    x <- x %||% default
    if (length(x) == 1L) x <- rep(x, n)
    if (length(x) != n) stop(label, " must have length 1 or length(files).", call. = FALSE)
    as.character(x)
  }
  source_type <- recycle(source_type, "source_type")
  authority <- recycle(authority, "authority")
  titles <- recycle(titles, "titles", basename(files))
  notes <- recycle(notes, "notes", "")
  if (is.null(source_ids)) {
    source_ids <- vapply(files, function(f) paste0("SRC-", toupper(substr(
      digest::digest(file = f, algo = "sha256"), 1L, 10L))), character(1))
  } else source_ids <- recycle(source_ids, "source_ids")
  existing <- vapply(project$manifest$sources %||% list(), function(x) x$source_id, character(1))
  if (any(source_ids %in% existing) || anyDuplicated(source_ids)) {
    stop("Source IDs must be unique within the project.", call. = FALSE)
  }
  records <- vector("list", n)
  for (i in seq_len(n)) {
    original <- normalizePath(files[[i]], winslash = "/", mustWork = TRUE)
    stored <- original
    if (isTRUE(copy_into_project)) {
      target <- file.path(project$path, "sources", basename(original))
      ok <- file.copy(original, target, overwrite = FALSE)
      if (!ok) stop("Could not copy source into project: ", original, call. = FALSE)
      stored <- normalizePath(target, winslash = "/", mustWork = TRUE)
    }
    records[[i]] <- list(
      source_id = source_ids[[i]], title = titles[[i]], source_type = source_type[[i]],
      authority = authority[[i]], original_path = original,
      project_path = lbsr_relative_path(stored, project$path),
      file_name = basename(stored), extension = tolower(tools::file_ext(stored)),
      size_bytes = unname(file.info(stored)$size),
      checksum_sha256 = digest::digest(file = stored, algo = "sha256"),
      registered_at = lbsr_now(), notes = notes[[i]]
    )
  }
  project$manifest$sources <- c(project$manifest$sources %||% list(), records)
  project$manifest$status <- "SOURCES_REGISTERED"
  write_lbsr_manifest(project)
}

