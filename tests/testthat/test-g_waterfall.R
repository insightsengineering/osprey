describe("tests for g_waterfall", {
  skip_if_not_installed("nestcolor")

  ADSL <- osprey::rADSL[1:15, ]
  ADRS <- osprey::rADRS |>
    dplyr::filter(USUBJID %in% ADSL$USUBJID)
  ADTR <- osprey::rADTR |>
    dplyr::filter(USUBJID %in% ADSL$USUBJID) |>
    dplyr::select(USUBJID, PCHG) |>
    dplyr::group_by(USUBJID) |>
    dplyr::slice(which.min(PCHG))

  TR_SL <- dplyr::inner_join(ADSL, ADTR, by = "USUBJID", multiple = "all") # nolint: object_name_linter,

  SUB_ADRS <- ADRS |> # nolint: object_name_linter,
    dplyr::filter(PARAMCD == "BESRSPI" | PARAMCD == "INVET") |>
    dplyr::select(USUBJID, PARAMCD, AVALC, AVISIT, ADY) |>
    tidyr::spread(PARAMCD, AVALC)

  ANL <- TR_SL |>
    dplyr::left_join(SUB_ADRS, by = "USUBJID", multiple = "all") |>
    dplyr::mutate(TRTDURD = as.integer(TRTEDTM - TRTSDTM) + 1)

  anno_txt_vars <- c("TRTDURD", "BESRSPI", "INVET", "SEX", "BMRKR2")

  it("works without bar colour", {
    p1 <- g_waterfall(
      bar_height = ANL$PCHG,
      bar_id = sub(".*-", "", ANL$USUBJID),
      col_by = ANL$SEX,
      sort_by = ANL$ARM,
      anno_txt = ANL[, anno_txt_vars],
      facet_by = NULL,
      href_line = c(-30, 20),
      add_label = ANL$BESRSPI,
      ytick_at = 20,
      gap_point = NULL,
      show_datavalue = TRUE,
      y_label = "Best % Change from Baseline",
      title = "Waterfall Plot"
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with color_bar_opt", {
    p1 <- g_waterfall(
      bar_height = ANL$PCHG,
      bar_id = sub(".*-", "", ANL$USUBJID),
      col_by = ANL$SEX,
      sort_by = ANL$ARM,
      bar_color_opt = c("F" = "red", "M" = "green", "U" = "blue"),
      anno_txt = ANL[, anno_txt_vars],
      facet_by = NULL,
      href_line = c(-30, 20),
      add_label = ANL$BESRSPI,
      ytick_at = 20,
      gap_point = NULL,
      show_datavalue = TRUE,
      y_label = "Best % Change from Baseline",
      title = "Waterfall Plot"
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with NA bar_height", {
    p1 <- g_waterfall(
      bar_height = rep(NA, length(ANL$PCHG)),
      bar_id = ANL$PCHG,
      col_by = ANL$SEX,
      sort_by = ANL$ARM,
      bar_color_opt = c("F" = "red", "M" = "green", "U" = "blue"),
      anno_txt = ANL[, anno_txt_vars],
      facet_by = NULL,
      href_line = c(-30, 20),
      add_label = ANL$BESRSPI,
      ytick_at = 20,
      gap_point = 260,
      show_datavalue = TRUE,
      y_label = "Best % Change from Baseline",
      title = "Waterfall Plot"
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with gap_point above bar_height", {
    p1 <- g_waterfall(
      bar_height = ANL$PCHG,
      bar_id = sub(".*-", "", ANL$USUBJID),
      col_by = ANL$SEX,
      sort_by = ANL$ARM,
      bar_color_opt = c("F" = "red", "M" = "green", "U" = "blue"),
      anno_txt = ANL[, anno_txt_vars],
      facet_by = NULL,
      href_line = c(-30, 20),
      add_label = ANL$BESRSPI,
      ytick_at = 20,
      gap_point = 210,
      show_datavalue = TRUE,
      y_label = "Best % Change from Baseline",
      title = "Waterfall Plot"
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with gap_point below bar_height", {
    p1 <- g_waterfall(
      bar_height = ANL$PCHG,
      bar_id = sub(".*-", "", ANL$USUBJID),
      col_by = ANL$SEX,
      sort_by = ANL$ARM,
      bar_color_opt = c("F" = "red", "M" = "green", "U" = "blue"),
      anno_txt = ANL[, anno_txt_vars],
      facet_by = NULL,
      href_line = c(-30, 20),
      add_label = ANL$BESRSPI,
      ytick_at = 20,
      gap_point = 15,
      show_datavalue = TRUE,
      y_label = "Best % Change from Baseline",
      title = "Waterfall Plot"
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works without sorting", {
    p1 <- g_waterfall(
      bar_height = ANL$PCHG,
      bar_id = sub(".*-", "", ANL$USUBJID),
      col_by = ANL$SEX,
      bar_color_opt = c("F" = "red", "M" = "green", "U" = "blue"),
      anno_txt = ANL[, anno_txt_vars],
      facet_by = NULL,
      href_line = c(-30, 20),
      add_label = ANL$BESRSPI,
      ytick_at = 20,
      gap_point = NULL,
      show_datavalue = TRUE,
      y_label = "Best % Change from Baseline",
      title = "Waterfall Plot"
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works without faceting", {
    p1 <- g_waterfall(
      bar_id = sub(".*-", "", ANL$USUBJID),
      bar_height = ANL$PCHG,
      sort_by = ANL$COUNTRY,
      col_by = ANL$SEX,
      bar_color_opt = c("F" = "tomato", "M" = "skyblue3", "U" = "darkgreen"),
      anno_txt = ANL[, anno_txt_vars],
      facet_by = ANL$STRATA2,
      href_line = c(-30, 20),
      add_label = ANL$BESRSPI,
      ytick_at = 20,
      gap_point = 260,
      y_label = "Best % Change from Baseline",
      title = "Waterfall Plot"
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })
})

describe("g_waterfall input verification", {
  ADSL <- osprey::rADSL[1:15, ]
  ADRS <- osprey::rADRS |>
    dplyr::filter(USUBJID %in% ADSL$USUBJID)
  ADTR <- osprey::rADTR |>
    dplyr::filter(USUBJID %in% ADSL$USUBJID) |>
    dplyr::select(USUBJID, PCHG) |>
    dplyr::group_by(USUBJID) |>
    dplyr::slice(which.min(PCHG))

  TR_SL <- dplyr::inner_join(ADSL, ADTR, by = "USUBJID", multiple = "all") # nolint: object_name_linter,

  SUB_ADRS <- ADRS |> # nolint: object_name_linter,
    dplyr::filter(PARAMCD == "BESRSPI" | PARAMCD == "INVET") |>
    dplyr::select(USUBJID, PARAMCD, AVALC, AVISIT, ADY) |>
    tidyr::spread(PARAMCD, AVALC)

  ANL <- TR_SL |>
    dplyr::left_join(SUB_ADRS, by = "USUBJID", multiple = "all") |>
    dplyr::mutate(TRTDURD = as.integer(TRTEDTM - TRTSDTM) + 1)

  anno_txt_vars <- c("TRTDURD", "BESRSPI", "INVET", "SEX", "BMRKR2")

  it("fails if bar_height or bad_id have wrong dimensions", {
    expect_error(
      {
        g_waterfall(
          bar_height = ANL$PCHG[1:2],
          bar_id = sub(".*-", "", ANL$USUBJID),
          col_by = ANL$SEX,
          sort_by = ANL$ARM,
          anno_txt = ANL[, anno_txt_vars],
          facet_by = NULL,
          href_line = c(-30, 20),
          add_label = ANL$BESRSPI,
          ytick_at = 20,
          gap_point = NULL,
          show_datavalue = TRUE,
          y_label = "Best % Change from Baseline",
          title = "Waterfall Plot"
        )
      },
      regexp = "check that the length of input arguments are identical"
    )
  })

  it("fails if bar_height or bad_id are NULL", {
    expect_error(
      {
        g_waterfall(
          bar_height = ANL$PCHG[1:2],
          bar_id = sub(".*-", "", ANL$USUBJID),
          col_by = ANL$SEX,
          sort_by = ANL$ARM,
          anno_txt = ANL[, anno_txt_vars],
          facet_by = NULL,
          href_line = c(-30, 20),
          add_label = ANL$BESRSPI,
          ytick_at = 20,
          gap_point = NULL,
          show_datavalue = TRUE,
          y_label = "Best % Change from Baseline",
          title = "Waterfall Plot"
        )
      },
      regexp = "check that the length of input arguments are identical"
    )
  })
})
