describe("tests for g_spiderplot", {
  ADTR <- osprey::rADTR |> select(STUDYID, USUBJID, ADY, AVISIT, CHG, PCHG, PARAMCD)
  ADSL <- osprey::rADSL |> select(STUDYID, USUBJID, RACE, SEX, ARM)
  ANL <- left_join(ADTR, ADSL, by = c("STUDYID", "USUBJID"))
  ANL <- ANL |>
    dplyr::filter(PARAMCD == "SLDINV" & AVISIT != "POST-BASELINE MINIMUM") |>
    dplyr::filter(RACE %in% c("WHITE", "ASIAN")) |>
    dplyr::group_by(USUBJID) |>
    dplyr::arrange(ADY) |>
    dplyr::mutate(
      CHG = ifelse(AVISIT == "Screening", 0, CHG),
      PCHG = ifelse(AVISIT == "Screening", 0, PCHG)
    )
  ANL$USUBJID <- substr(ANL$USUBJID, 14, 18)

  it("works with default arguments", {
    p1 <- suppressWarnings(g_spiderplot(
      marker_x = ANL$ADY,
      marker_id = ANL$USUBJID,
      marker_y = ANL$PCHG,
      line_colby = ANL$USUBJID,
      marker_shape = ANL$USUBJID,
      datalabel_txt = list(txt_ann = ANL$USUBJID),
      vref_line = c(42, 86),
      href_line = c(-20, 20),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = TRUE
    ))
    expect_s3_class(p1, "ggplot")
  })

  it("works with line color mapping", {
    p1 <- g_spiderplot(
      marker_x = ANL$AVISIT,
      marker_id = ANL$USUBJID,
      marker_y = ANL$CHG,
      line_colby = ANL$RACE,
      line_color_opt = c("WHITE" = "red", "ASIAN" = "blue"),
      marker_shape = ANL$USUBJID,
      x_label = "Visit",
      y_label = "Change from Baseline",
      show_legend = TRUE
    )
    expect_s3_class(p1, "ggplot")
  })

  it("works with full faceting", {
    p1 <- g_spiderplot(
      marker_x = ANL$AVISIT,
      marker_id = ANL$USUBJID,
      marker_y = ANL$CHG,
      line_colby = ANL$RACE,
      line_color_opt = c("WHITE" = "red", "ASIAN" = "blue"),
      marker_shape = ANL$USUBJID,
      x_label = "Visit",
      y_label = "Change from Baseline",
      show_legend = TRUE,
      facet_columns = ANL$SEX,
      facet_rows = ANL$AVISIT
    )
    expect_s3_class(p1, "ggplot")
  })

  it("works with row faceting", {
    p1 <- g_spiderplot(
      marker_x = ANL$AVISIT,
      marker_id = ANL$USUBJID,
      marker_y = ANL$CHG,
      line_colby = ANL$RACE,
      line_color_opt = c("WHITE" = "red", "ASIAN" = "blue"),
      marker_shape = ANL$USUBJID,
      x_label = "Visit",
      y_label = "Change from Baseline",
      show_legend = TRUE,
      facet_rows = ANL$AVISIT
    )
    expect_s3_class(p1, "ggplot")
  })

  it("works with col faceting", {
    p1 <- g_spiderplot(
      marker_x = ANL$AVISIT,
      marker_id = ANL$USUBJID,
      marker_y = ANL$CHG,
      line_colby = ANL$RACE,
      line_color_opt = c("WHITE" = "red", "ASIAN" = "blue"),
      marker_shape = ANL$USUBJID,
      x_label = "Visit",
      y_label = "Change from Baseline",
      show_legend = TRUE,
      facet_columns = ANL$SEX
    )
    expect_s3_class(p1, "ggplot")
  })

  it("works with NULL market_shape", {
    p1 <- g_spiderplot(
      marker_x = ANL$AVISIT,
      marker_id = ANL$USUBJID,
      marker_y = ANL$CHG,
      line_colby = ANL$RACE,
      line_color_opt = c("WHITE" = "red", "ASIAN" = "blue"),
      marker_shape = NULL,
      x_label = "Visit",
      y_label = "Change from Baseline",
      show_legend = TRUE
    )
    expect_s3_class(p1, "ggplot")
  })

  it("works with defined market_shape_opt", {
    p1 <- g_spiderplot(
      marker_x = ANL$AVISIT,
      marker_id = ANL$USUBJID,
      marker_y = ANL$CHG,
      line_colby = ANL$RACE,
      line_color_opt = c("WHITE" = "red", "ASIAN" = "blue"),
      marker_shape = ANL$SEX,
      marker_shape_opt = c("F" = 8, "M" = 9),
      x_label = "Visit",
      y_label = "Change from Baseline",
      show_legend = TRUE
    )
    expect_s3_class(p1, "ggplot")
  })

  it("works with defined txt_mrkr_all", {
    p1 <- suppressWarnings(g_spiderplot(
      marker_x = ANL$ADY,
      marker_id = ANL$USUBJID,
      marker_y = ANL$PCHG,
      line_colby = ANL$USUBJID,
      marker_shape = ANL$USUBJID,
      datalabel_txt = list(mrkr_ann = ANL$AVISIT, mrkr_all = ANL$STUDYID),
      vref_line = c(42, 86),
      href_line = c(-20, 20),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = TRUE
    ))
    expect_s3_class(p1, "ggplot")
  })

  it("works with full defined datalabel_txt", {
    p1 <- suppressWarnings(g_spiderplot(
      marker_x = ANL$ADY,
      marker_id = ANL$USUBJID,
      marker_y = ANL$PCHG,
      line_colby = ANL$USUBJID,
      marker_shape = ANL$USUBJID,
      datalabel_txt = list(txt_ann = ANL$USUBJID, mrkr_ann = ANL$AVISIT, mrkr_all = ANL$STUDYID),
      vref_line = c(42, 86),
      href_line = c(-20, 20),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = TRUE
    ))
    expect_s3_class(p1, "ggplot")
  })
})

describe("input validation tests for g_spiderplot", {
  ADTR <- osprey::rADTR |> select(STUDYID, USUBJID, ADY, AVISIT, CHG, PCHG, PARAMCD)
  ADSL <- osprey::rADSL |> select(STUDYID, USUBJID, RACE, SEX, ARM)
  ANL <- left_join(ADTR, ADSL, by = c("STUDYID", "USUBJID"))
  ANL <- ANL |>
    dplyr::filter(PARAMCD == "SLDINV" & AVISIT != "POST-BASELINE MINIMUM") |>
    dplyr::filter(RACE %in% c("WHITE", "ASIAN")) |>
    dplyr::group_by(USUBJID) |>
    dplyr::arrange(ADY) |>
    dplyr::mutate(
      CHG = ifelse(AVISIT == "Screening", 0, CHG),
      PCHG = ifelse(AVISIT == "Screening", 0, PCHG)
    )
  ANL$USUBJID <- substr(ANL$USUBJID, 14, 18)

  it("fails if assumptions on marker_x, marker_y and marker_id are not valid", {
    expect_error(g_spiderplot(
      marker_x = rep("random", 5),
      marker_id = ANL$USUBJID,
      marker_y = ANL$PCHG,
      line_colby = ANL$USUBJID,
      marker_shape = ANL$USUBJID,
      datalabel_txt = list(txt_ann = ANL$USUBJID, mrkr_ann = ANL$AVISIT, mrkr_all = ANL$STUDYID),
      vref_line = c(42, 86),
      href_line = c(-20, 20),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = TRUE
    ), regexp = "length of input arguments are identical")
  })

  it("fails if assumptions on marker_x, marker_y and marker_id are not valid", {
    expect_error(g_spiderplot(
      marker_x = character(0),
      marker_id = character(0),
      marker_y = character(0),
      line_colby = ANL$USUBJID,
      marker_shape = ANL$USUBJID,
      datalabel_txt = list(txt_ann = ANL$USUBJID, mrkr_ann = ANL$AVISIT, mrkr_all = ANL$STUDYID),
      vref_line = c(42, 86),
      href_line = c(-20, 20),
      x_label = "Time (Days)",
      y_label = "Change (%) from Baseline",
      show_legend = TRUE
    ), regexp = "check that inputs are not null")
  })
})
