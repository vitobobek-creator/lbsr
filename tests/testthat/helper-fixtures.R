make_valid_tables <- function() {
  schemas <- lbsr_schema()
  blank <- lapply(schemas, function(s) {
    out <- as.data.frame(setNames(replicate(length(s$required), character(), simplify = FALSE),
                                 s$required), stringsAsFactors = FALSE)
    out
  })
  blank$items <- data.frame(
    code = "ATT_TEC1", hypothesis = "H1", tpb_block_code = "ATT",
    tpb_block = "Attitude", dimension_code = "TEC", dimension = "Technical",
    item_no = 1, original_item_wording = "Example item", n = 66,
    mean = 3.55, sd = 0.90, source_table = "Table 24",
    internal_source_location = "source", reconstruction_status = "confirmed",
    stringsAsFactors = FALSE
  )
  blank$composite_frequencies <- data.frame(
    composite_code="ATT_TEC",hypothesis="H1",tpb_block_code="ATT",tpb_block="Attitude",
    dimension_code="TEC",dimension="Technical",response_category="1",frequency=6,
    percent=3,valid_percent=3,cumulative_percent=3,internal_source_location="source"
  )
  blank$group_means <- data.frame(
    composite_code="ATT_TEC",hypothesis="H1",tpb_block_code="ATT",tpb_block="Attitude",
    dimension_code="TEC",dimension="Technical",pv_ownership_group="NE",mean=3.55,
    n=174,sd=.896,internal_source_location="source"
  )
  blank$hypothesis_tests <- data.frame(
    composite_code="ATT_TEC",hypothesis="H1",tpb_block_code="ATT",tpb_block="Attitude",
    dimension_code="TEC",dimension="Technical",test="Pearson Chi-square",statistic=22.336,
    p_value=0,internal_source_location="source"
  )
  blank$reconstruction_plan <- data.frame(stage=1,action="Import",status="Completed",notes="")
  blank
}

make_multivariate_bundle <- function() {
  z <- make_valid_tables()
  z$items <- rbind(z$items, transform(z$items, code = "ATT_TEC2", item_no = 2,
                                      mean = 3.25, sd = 1.00))
  z$composite_frequencies <- data.frame(
    composite_code = rep("ATT_TEC", 5), hypothesis = "H1", tpb_block_code = "ATT",
    tpb_block = "Attitude", dimension_code = "TEC", dimension = "Technical",
    response_category = c("Sploh se ne strinjam", "Se ne strinjam", "Niti niti",
                          "Se strinjam", "Se popolnoma strinjam"),
    frequency = c(10, 20, 30, 40, 32),
    percent = NA_real_, valid_percent = NA_real_, cumulative_percent = NA_real_,
    internal_source_location = "fixture", stringsAsFactors = FALSE)
  new_lbsr_evidence_bundle(z$items, z$composite_frequencies, z$group_means,
                           z$hypothesis_tests, z$reconstruction_plan)
}
