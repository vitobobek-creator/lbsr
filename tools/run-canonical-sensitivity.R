args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript tools/run-canonical-sensitivity.R CODING_BOOK [OUTPUT_DIR]",
       call. = FALSE)
}
coding_book <- normalizePath(args[[1]], mustWork = TRUE)
output_dir <- if (length(args) >= 2L) args[[2]] else "lbsr-sensitivity-output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("lbsr", quietly = TRUE)) {
  stop("Install the local lbsr package before running this script.", call. = FALSE)
}

bundle <- lbsr::import_coding_book(coding_book)
if (!bundle$validation$valid) {
  stop("The coding book failed schema validation.", call. = FALSE)
}

ensemble <- lbsr::reconstruct_ensemble(
  bundle, respondent_n = 66, seeds = 1:5,
  reliability_values = c(0.60, 0.70, 0.80),
  correlation_values = c(0.15, 0.25, 0.35),
  group_sizes = c(NE = 58, DA = 8), max_runs = 45,
  keep_reconstructions = TRUE)

lbsr::write_sensitivity_report(
  ensemble, file.path(output_dir, "LBSR_sensitivity_report.md"),
  title = "REC reconstruction sensitivity analysis")
utils::write.csv(ensemble$registry,
                 file.path(output_dir, "run_registry.csv"), row.names = FALSE)
utils::write.csv(ensemble$summary$target_stability,
                 file.path(output_dir, "target_stability.csv"), row.names = FALSE)
utils::write.csv(ensemble$summary$parameter_effects,
                 file.path(output_dir, "parameter_effects.csv"), row.names = FALSE)
saveRDS(ensemble, file.path(output_dir, "sensitivity_ensemble.rds"), version = 3)

selected <- lbsr::select_sensitivity_run(ensemble)
utils::write.csv(selected$data,
                 file.path(output_dir, "selected_synthetic_reconstruction.csv"),
                 row.names = FALSE)
message("Sensitivity outputs written to: ", normalizePath(output_dir, winslash = "/"))
