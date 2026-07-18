lbsr_schema <- function(name = NULL) {
  schemas <- list(
    items = list(
      required = c("code", "hypothesis", "tpb_block_code", "tpb_block",
                   "dimension_code", "dimension", "item_no", "original_item_wording",
                   "n", "mean", "sd", "source_table", "internal_source_location",
                   "reconstruction_status"),
      key = "code",
      numeric = c("item_no", "n", "mean", "sd")
    ),
    composite_frequencies = list(
      required = c("composite_code", "hypothesis", "tpb_block_code", "tpb_block",
                   "dimension_code", "dimension", "response_category", "frequency",
                   "percent", "valid_percent", "cumulative_percent",
                   "internal_source_location"),
      key = NULL,
      numeric = c("frequency", "percent", "valid_percent", "cumulative_percent")
    ),
    group_means = list(
      required = c("composite_code", "hypothesis", "tpb_block_code", "tpb_block",
                   "dimension_code", "dimension", "pv_ownership_group", "mean", "n",
                   "sd", "internal_source_location"),
      key = NULL,
      numeric = c("mean", "n", "sd")
    ),
    hypothesis_tests = list(
      required = c("composite_code", "hypothesis", "tpb_block_code", "tpb_block",
                   "dimension_code", "dimension", "test", "statistic", "p_value",
                   "internal_source_location"),
      key = NULL,
      numeric = c("statistic", "p_value")
    ),
    reconstruction_plan = list(
      required = c("stage", "action", "status", "notes"),
      key = "stage",
      numeric = c("stage")
    )
  )
  if (is.null(name)) return(schemas)
  if (!name %in% names(schemas)) stop("Unknown schema: ", name, call. = FALSE)
  schemas[[name]]
}

lbsr_findings <- function() {
  data.frame(code = character(), severity = character(), table = character(),
             field = character(), row = integer(), message = character(),
             stringsAsFactors = FALSE)
}

add_finding <- function(findings, code, severity, table, field = "",
                        row = NA_integer_, message) {
  rbind(findings, data.frame(
    code = code, severity = toupper(severity), table = table, field = field,
    row = as.integer(row), message = message, stringsAsFactors = FALSE
  ))
}

validate_table_schema <- function(x, schema_name) {
  schema <- lbsr_schema(schema_name)
  findings <- lbsr_findings()
  if (!is.data.frame(x)) {
    return(add_finding(findings, "LBSR-SCHEMA-010", "ERROR", schema_name,
                       message = "Imported object is not a data frame."))
  }
  missing <- setdiff(schema$required, names(x))
  extra <- setdiff(names(x), schema$required)
  for (field in missing) findings <- add_finding(
    findings, "LBSR-SCHEMA-011", "ERROR", schema_name, field,
    message = paste("Missing required column:", field)
  )
  for (field in extra) findings <- add_finding(
    findings, "LBSR-SCHEMA-012", "INFO", schema_name, field,
    message = paste("Additional column retained:", field)
  )
  if (!length(missing)) {
    for (field in schema$numeric) {
      bad <- which(!is.na(x[[field]]) & is.na(suppressWarnings(as.numeric(x[[field]]))))
      for (i in bad) findings <- add_finding(
        findings, "LBSR-SCHEMA-013", "ERROR", schema_name, field, i,
        paste("Value is not numeric:", x[[field]][[i]])
      )
    }
    if (!is.null(schema$key)) {
      key <- x[[schema$key]]
      empty <- which(is.na(key) | !nzchar(trimws(as.character(key))))
      dup <- which(duplicated(key) & !is.na(key))
      for (i in empty) findings <- add_finding(findings, "LBSR-SCHEMA-014", "ERROR",
        schema_name, schema$key, i, "Key value is missing or empty.")
      for (i in dup) findings <- add_finding(findings, "LBSR-SCHEMA-015", "ERROR",
        schema_name, schema$key, i, "Duplicate key value.")
    }
  }
  findings
}

