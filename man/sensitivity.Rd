\name{sensitivity}
\alias{reconstruct_ensemble}
\alias{summarize_sensitivity}
\alias{select_sensitivity_run}
\alias{write_sensitivity_report}
\title{Reconstruction sensitivity ensembles}
\usage{
reconstruct_ensemble(x, respondent_n, seeds = 1:10,
  reliability_values = c(0.60, 0.70, 0.80),
  correlation_values = c(0.15, 0.25, 0.35), group_sizes = NULL,
  group_tolerance = 0.15, max_swaps = 10000L, max_runs = 100L,
  keep_reconstructions = TRUE)

summarize_sensitivity(x)

select_sensitivity_run(x, rule = c("min_normalized_loss", "max_pass_rate"))

write_sensitivity_report(x, file, title = "LBSR sensitivity analysis report")
}
\arguments{
  \item{x}{An evidence bundle or sensitivity object, as appropriate.}
  \item{respondent_n}{Synthetic records per run.}
  \item{seeds}{Integer random seeds.}
  \item{reliability_values}{Cronbach alpha evaluation assumptions.}
  \item{correlation_values}{Within-composite association assumptions.}
  \item{group_sizes}{Optional named group sizes.}
  \item{group_tolerance}{Group-mean calibration tolerance.}
  \item{max_swaps}{Maximum group-calibration swaps.}
  \item{max_runs}{Safety limit for grid size.}
  \item{keep_reconstructions}{Whether complete runs are retained.}
  \item{rule}{Transparent preferred-run selection rule.}
  \item{file}{Output Markdown path.}
  \item{title}{Report title.}
}
\value{An ensemble, sensitivity summary, selected reconstruction, or report path.}
\description{Quantifies sensitivity to seeds and declared association assumptions without
matching synthetic rows across runs. Comparisons operate on aggregate evidence targets.}
