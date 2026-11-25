describe("tests for g_butterfly", {
  ADSL <- osprey::rADSL |>
    select(USUBJID, STUDYID, SEX, ARM, RACE) |>
    dplyr::filter(SEX %in% c("F", "M"))
  ADAE <- osprey::rADAE |> select(USUBJID, STUDYID, AEBODSYS, AETOXGR)

  ANL <- left_join(ADAE, ADSL, by = c("STUDYID", "USUBJID"))
  ANL <- ANL |>
    dplyr::mutate(flag1 = ifelse(RACE == "ASIAN", 1, 0)) |>
    dplyr::mutate(flag2 = ifelse(SEX == "M", 1, 0))
  ANL <- na.omit(ANL)

  it("works with standard parameters", {
    p1 <- g_butterfly(
      category = ANL$AEBODSYS,
      right_flag = ANL$flag1,
      left_flag = ANL$flag2,
      group_names = c("flag1 Asian", "flag2 M"),
      block_count = "# of AEs",
      block_color = ANL$AETOXGR,
      id = ANL$USUBJID,
      x_label = "# of AEs",
      y_label = "AE Body System",
      legend_label = "AETOXGR",
      sort_by = "count",
      show_legend = TRUE
    )

  expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with alphabetical sorting", {
    p1 <- g_butterfly(
      category = ANL$AEBODSYS,
      right_flag = ANL$flag1,
      left_flag = ANL$flag2,
      group_names = c("flag1 Asian", "flag2 M"),
      block_count = "# of AEs",
      block_color = ANL$AETOXGR,
      id = ANL$USUBJID,
      x_label = "# of AEs",
      y_label = "AE Body System",
      legend_label = "AETOXGR",
      sort_by = "alphabetical",
      show_legend = TRUE
    )

    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with facet rows", {
    p1 <- g_butterfly(
      category = ANL$AEBODSYS,
      right_flag = ANL$flag1,
      left_flag = ANL$flag2,
      group_names = c("flag1 Asian", "flag2 M"),
      block_count = "# of patients",
      block_color = ANL$AETOXGR,
      facet_rows = ANL$ARM,
      id = ANL$USUBJID,
      x_label = "# of patients",
      y_label = "AE Derived Terms",
      legend_label = "AETOXGR",
      sort_by = "count",
      show_legend = TRUE
    )

    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with left sorting and no block_color", {
    p1 <- g_butterfly(
      category = ANL$AEBODSYS,
      right_flag = ANL$flag1,
      left_flag = ANL$flag2,
      group_names = c("flag1 Asian", "flag2 M"),
      block_count = "# of patients",
      block_color = NULL,
      id = ANL$USUBJID,
      x_label = "# of patients",
      y_label = "AE Derived Terms",
      legend_label = "AETOXGR",
      sort_by = "left",
      show_legend = TRUE
    )

    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })
})
