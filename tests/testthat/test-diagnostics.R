make_diagnostic_bundle <- function() {
  items <- data.frame(
    code = c("ATT_ENV1","ATT_ENV2","ATT_ENV3","ATT_TEC1","ATT_TEC2","ATT_TEC3"),
    hypothesis = "H1", tpb_block_code = "ATT", tpb_block = "Attitude",
    dimension_code = rep(c("ENV","TEC"), each = 3),
    dimension = rep(c("Environmental","Technical"), each = 3),
    item_no = rep(1:3, 2), original_item_wording = paste("Item", 1:6),
    n = 66, mean = rep(c(3.55,3.58,3.73), 2),
    sd = rep(c(.898,.878,.869), 2), source_table = "table",
    internal_source_location = "source", reconstruction_status = "reported",
    stringsAsFactors = FALSE
  )
  group <- do.call(rbind, lapply(c("ATT_ENV","ATT_TEC"), function(code) data.frame(
    composite_code=code,hypothesis="H1",tpb_block_code="ATT",tpb_block="Attitude",
    dimension_code=substring(code,5),dimension=substring(code,5),
    pv_ownership_group=c("NE","DA","Skupaj"),
    mean=if(code=="ATT_ENV") c(4.02,4.13,4.03) else c(3.55,4.29,3.64),
    n=c(174,24,198),sd=c(.896,.624,.900),internal_source_location="source"
  )))
  tests <- data.frame(
    composite_code=c("ATT_ENV","ATT_TEC"),hypothesis="H1",tpb_block_code="ATT",
    tpb_block="Attitude",dimension_code=c("ENV","TEC"),dimension=c("Environmental","Technical"),
    test="Pearson Chi-square",statistic=c(3.736,22.336),p_value=c(.443,0),
    internal_source_location="source", stringsAsFactors=FALSE
  )
  composite_frequencies <- data.frame(
    composite_code="ATT_TEC",hypothesis="H1",tpb_block_code="ATT",tpb_block="Attitude",
    dimension_code="TEC",dimension="Technical",response_category="Total",frequency=198,
    percent=100,valid_percent=100,cumulative_percent=100,internal_source_location="source"
  )
  reconstruction_plan <- data.frame(stage=1,action="Import",status="Completed",notes="")
  lbsr:::new_lbsr_evidence_bundle(items, composite_frequencies, group, tests,
                                  reconstruction_plan)
}

test_that("sample-size diagnostics recover group respondents", {
  d <- diagnose_sample_sizes(make_diagnostic_bundle(), respondent_n = 66)
  expect_s3_class(d, "lbsr_diagnostics")
  ne <- d$sample_sizes[d$sample_sizes$composite_code=="ATT_TEC" &
                       d$sample_sizes$group=="NE",]
  da <- d$sample_sizes[d$sample_sizes$composite_code=="ATT_TEC" &
                       d$sample_sizes$group=="DA",]
  expect_equal(ne$inferred_respondents, 58)
  expect_equal(da$inferred_respondents, 8)
  expect_true("LBSR-B001" %in% d$issues$code)
})

test_that("conflicts and duplicate patterns are detected", {
  d <- detect_source_conflicts(make_diagnostic_bundle(),
                               claimed_item_counts = c(28, 6))
  expect_true(any(d$conflicts$target == "ATT_ENV"))
  expect_true(any(d$conflicts$conflict_type == "inventory_item_count"))
  expect_true(any(d$duplicates$composite_a == "ATT_ENV" |
                  d$duplicates$composite_b == "ATT_ENV"))
})

test_that("combined diagnostics retain both components", {
  d <- run_lbsr_diagnostics(make_diagnostic_bundle(), respondent_n = 66,
                            claimed_item_counts = 28)
  expect_gt(nrow(d$sample_sizes), 0)
  expect_gt(nrow(d$conflicts), 0)
  expect_gt(nrow(d$issues), 0)
})

