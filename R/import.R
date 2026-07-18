read_lbsr_sheet <- function(file, sheet) {
  x <- as.data.frame(readxl::read_excel(file, sheet = sheet, .name_repair = "minimal"),
                     stringsAsFactors = FALSE)
  names(x) <- normalize_lbsr_names(names(x))
  x <- x[, !grepl("^\.\.\.[0-9]+$|^$", names(x)), drop = FALSE]
  x
}

import_coding_book <- function(file, project = NULL, strict = TRUE,
                               expected_items = 62L,
                               expected_composites = 16L) {
  lbsr_scalar_character(file, "file")
  if (!file.exists(file)) stop("Coding book not found: ", file, call. = FALSE)
  if (tolower(tools::file_ext(file)) != "xlsx") {
    stop("Version 0.2.0 imports the master coding book from .xlsx files.", call. = FALSE)
  }
  sheets <- readxl::excel_sheets(file)
  missing <- setdiff(.lbsr_required_sheets, sheets)
  if (length(missing)) {
    stop("Missing required workbook sheet(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  tables <- lapply(.lbsr_sheet_aliases, function(sheet) read_lbsr_sheet(file, sheet))
  metadata <- list(
    imported_at = lbsr_now(), file_name = basename(file),
    source_path = normalizePath(file, winslash = "/", mustWork = TRUE),
    checksum_sha256 = digest::digest(file = file, algo = "sha256"),
    workbook_sheets = sheets,
    data_status = "derived_evidence_inventory"
  )
  bundle <- do.call(new_lbsr_evidence_bundle, c(tables, list(metadata = metadata)))
  structural <- validate_coding_book_schema(bundle)
  if (isTRUE(strict) && !structural$valid) {
    msgs <- structural$findings$message[structural$findings$severity %in% c("ERROR", "CRITICAL")]
    stop("Coding-book schema validation failed:\n- ", paste(msgs, collapse = "\n- "), call. = FALSE)
  }
  bundle$validation <- validate_evidence_bundle(
    bundle, expected_items = expected_items,
    expected_composites = expected_composites
  )
  class(bundle) <- "lbsr_evidence_bundle"
  if (!is.null(project)) {
    if (!inherits(project, "lbsr_project")) stop("project must be an lbsr_project.", call. = FALSE)
    out <- file.path(project$path, "derived", "coding_book_import_summary.json")
    summary <- list(
      source = metadata,
      counts = list(items = nrow(bundle$items),
                    composites = length(unique(paste(bundle$items$tpb_block_code,
                                                     bundle$items$dimension_code, sep = "_"))),
                    group_means = nrow(bundle$group_means),
                    hypothesis_tests = nrow(bundle$hypothesis_tests)),
      validation = list(valid = bundle$validation$valid,
                        findings = bundle$validation$findings)
    )
    writeLines(jsonlite::toJSON(summary, auto_unbox = TRUE, pretty = TRUE,
                                null = "null", na = "null"), out, useBytes = TRUE)
  }
  bundle
}
