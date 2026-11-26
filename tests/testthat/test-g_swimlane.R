describe("test for swimlane", {
  ADSL <- osprey::rADSL[1:20, ]
  ADRS <- dplyr::filter(rADRS, PARAMCD == "OVRINV")
  ANL <- dplyr::left_join(ADSL, ADRS, by = c("STUDYID", "USUBJID"), multiple = "all")
  anno_txt <- ADSL[, c("ARMCD", "SEX")]

  it("works with default arguments", {
    p1 <- g_swimlane(
      bar_id = ADSL$USUBJID,
      bar_length = as.integer(ADSL$TRTEDTM - ADSL$TRTSDTM),
      sort_by = ADSL$ARM,
      col_by = ADSL$ARM,
      marker_id = ANL$USUBJID,
      marker_pos = ANL$ADY,
      marker_shape = ANL$AVALC,
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
      marker_color = NULL,
      marker_color_opt = NULL,
      anno_txt = anno_txt,
      xref_line = c(50, 100),
      xtick_at = waiver(),
      xlab = "Time from First Treatment (Day)",
      title = "Swimlane Plot"
    )

    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works without setting col_by or sort_by", {
    p1 <- g_swimlane(
      bar_id = ADSL$USUBJID,
      bar_length = as.integer(ADSL$TRTEDTM - ADSL$TRTSDTM),
      sort_by = NULL,
      col_by = NULL,
      marker_id = ANL$USUBJID,
      marker_pos = ANL$ADY,
      marker_shape = ANL$AVALC,
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
      marker_color = NULL,
      marker_color_opt = NULL,
      anno_txt = anno_txt,
      xref_line = c(50, 100),
      xtick_at = waiver(),
      xlab = "Time from First Treatment (Day)",
      title = "Swimlane Plot"
    )

    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with defined marker_color", {
    p1 <- g_swimlane(
      bar_id = ADSL$USUBJID,
      bar_length = as.integer(ADSL$TRTEDTM - ADSL$TRTSDTM),
      sort_by = ADSL$ARM,
      col_by = ADSL$ARM,
      marker_id = ANL$USUBJID,
      marker_pos = ANL$ADY,
      marker_shape = ANL$AVALC,
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
      marker_color = rep("blue", length(ANL$ADY)),
      marker_color_opt = NULL,
      anno_txt = anno_txt,
      xref_line = c(50, 100),
      xtick_at = waiver(),
      xlab = "Time from First Treatment (Day)",
      title = "Swimlane Plot"
    )

    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })  
})

describe("input validation tests for g_swimlane", {
  ADSL <- osprey::rADSL[1:20, ]
  ADRS <- dplyr::filter(rADRS, PARAMCD == "OVRINV")
  ANL <- dplyr::left_join(ADSL, ADRS, by = c("STUDYID", "USUBJID"), multiple = "all")
  anno_txt <- ADSL[, c("ARMCD", "SEX")]

  it("fails if marker_id does not follow assumption on bar_id", {
    expect_error(g_swimlane(
      bar_id = ADSL$USUBJID,
      bar_length = as.integer(ADSL$TRTEDTM - ADSL$TRTSDTM),
      sort_by = ADSL$ARM,
      col_by = ADSL$ARM,
      marker_id = rep("random", length(ANL$ADY)),
      marker_pos = ANL$ADY,
      marker_shape = ANL$AVALC,
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
      marker_color = NULL,
      marker_color_opt = NULL,
      anno_txt = anno_txt,
      xref_line = c(50, 100),
      xtick_at = waiver(),
      xlab = "Time from First Treatment (Day)",
      title = "Swimlane Plot"
    ), regexp = "is not in bar_id")
  })

  it("fails if xref_line is not of the expected type", {
    expect_error(g_swimlane(
      bar_id = ADSL$USUBJID,
      bar_length = as.integer(ADSL$TRTEDTM - ADSL$TRTSDTM),
      sort_by = ADSL$ARM,
      col_by = ADSL$ARM,
      marker_id = ANL$USUBJID,
      marker_pos = ANL$ADY,
      marker_shape = ANL$AVALC,
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
      marker_color = NULL,
      marker_color_opt = NULL,
      anno_txt = anno_txt,
      xref_line = "random_line",
      xtick_at = waiver(),
      xlab = "Time from First Treatment (Day)",
      title = "Swimlane Plot"
    ), regexp = "non-empty numeric vector")
  })

  it("fails if marker_color does not have the expected length", {
    expect_error(g_swimlane(
      bar_id = ADSL$USUBJID,
      bar_length = as.integer(ADSL$TRTEDTM - ADSL$TRTSDTM),
      sort_by = ADSL$ARM,
      col_by = ADSL$ARM,
      marker_id = ANL$USUBJID,
      marker_pos = ANL$ADY,
      marker_shape = ANL$AVALC,
      marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
      marker_color = "blue",
      marker_color_opt = NULL,
      anno_txt = anno_txt,
      xref_line = c(50, 100),
      xtick_at = waiver(),
      xlab = "Time from First Treatment (Day)",
      title = "Swimlane Plot"
    ), regexp = "dimension mismatch")
  })
})
