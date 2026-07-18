.lbsr_manifest_version <- "1.0"
.lbsr_package_phase <- "phase_2_ingestion_and_schema"

.lbsr_directories <- c(
  "sources",
  "config",
  "derived",
  "reports",
  "exports",
  "logs"
)

.lbsr_statuses <- c(
  "CONFIRMED", "INFERRED", "CONFLICTED", "UNAVAILABLE",
  "SUPERSEDED", "EXCLUDED"
)

.lbsr_measurement_modes <- c(
  "reflective", "formative", "composite", "single_item",
  "not_specified", "not_applicable"
)

.lbsr_required_sheets <- c(
  "Master Coding Book",
  "Composite Frequencies",
  "Group Means",
  "Hypothesis Tests",
  "Reconstruction Plan"
)

.lbsr_sheet_aliases <- c(
  items = "Master Coding Book",
  composite_frequencies = "Composite Frequencies",
  group_means = "Group Means",
  hypothesis_tests = "Hypothesis Tests",
  reconstruction_plan = "Reconstruction Plan"
)

