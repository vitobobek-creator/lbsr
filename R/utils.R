`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

lbsr_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "")
}

normalize_lbsr_names <- function(x) {
  x <- trimws(as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  make.unique(x, sep = "_")
}

lbsr_empty_df <- function(columns) {
  out <- as.data.frame(
    setNames(replicate(length(columns), character(), simplify = FALSE), columns),
    stringsAsFactors = FALSE
  )
  out
}

lbsr_scalar_character <- function(x, name, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(name, " must be one non-missing character value.", call. = FALSE)
  }
  if (!allow_empty && !nzchar(trimws(x))) {
    stop(name, " must not be empty.", call. = FALSE)
  }
  invisible(TRUE)
}

lbsr_relative_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

