describe("tests for patient profile_domain", {
  ADSL <- osprey::rADSL |>
    dplyr::filter(USUBJID == rADSL$USUBJID[1]) |>
    dplyr::mutate(
      TRTSDT = as.Date(TRTSDTM),
      max_date = max(as.Date(LSTALVDT), as.Date(DTHDT), na.rm = TRUE),
      max_day = as.numeric(as.Date(max_date) - as.Date(TRTSDT)) + 1
    ) |>
    dplyr::select(USUBJID, STUDYID, TRTSDT, max_day)

  ADEX <- osprey::rADEX |>
    dplyr::select(USUBJID, STUDYID, ASTDTM, PARCAT2, AVAL, AVALU, PARAMCD)
  ADEX <- left_join(ADSL, ADEX, by = c("USUBJID", "STUDYID"))
  ADEX <- ADEX |>
    dplyr::filter(PARAMCD == "DOSE") |>
    dplyr::arrange(PARCAT2, PARAMCD) |>
    dplyr::mutate(diff = c(0, diff(AVAL, lag = 1))) |>
    dplyr::mutate(
      Modification = case_when(
        diff < 0 ~ "Decrease",
        diff > 0 ~ "Increase",
        diff == 0 ~ "None"
      )
    ) |>
    dplyr::mutate(
      ASTDT_dur = as.numeric(
        as.Date(
          substr(as.character(ASTDTM), 1, 10)
        ) - as.Date(TRTSDT) + 1
      )
    )

  it("works with default parameters", {
    p1 <- patient_domain_profile(
      domain = "Exposure (ADEX)",
      var_names = ADEX$PARCAT2,
      marker_pos = ADEX$ASTDT_dur,
      arrow_end = ADSL$max_day,
      xtick_at = waiver(),
      line_col_list = NULL,
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0,
      marker_col_list = list(
        marker_col = factor(ADEX$Modification),
        marker_col_opt = c("Increase" = "red", "Decrease" = "green", "None" = "blue"),
        marker_col_legend = NULL
      ),
      marker_shape_list = list(
        marker_shape = factor(ADEX$Modification),
        marker_shape_opt = c("Increase" = 24, "Decrease" = 25, "None" = 23),
        marker_shape_legend = "Dose Modification"
      ),
      show_days_label = TRUE,
      xlim = c(-28, ADSL$max_day),
      xlab = "Study Day",
      title = paste("Patient Profile: ", ADSL$USUBJID)
    )
    expect_s3_class(p1, "ggplot")
  })

  it("works with the adverse event domain", {
    ADAE <- osprey::rADAE |>
      dplyr::select(USUBJID, STUDYID, AESOC, AEDECOD, AESER, AETOXGR, AEREL, ASTDY, AENDY)
    ADAE <- dplyr::left_join(ADSL, ADAE, by = c("USUBJID", "STUDYID"))

    p1 <- patient_domain_profile(
      domain = "Adverse Event (ADAE)",
      var_names = ADAE$AEDECOD,
      marker_pos = ADAE[, c("ASTDY", "AENDY")],
      arrow_end = ADSL$max_day,
      xtick_at = waiver(),
      line_col_list = list(
        line_col = ADAE$AESER,
        line_col_legend = "Serious",
        line_col_opt = c("blue", "green")
      ),
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0,
      marker_col_list = list(
        marker_col = factor(ADAE$AETOXGR),
        marker_col_opt = c("3" = "yellow", "4" = "red"),
        marker_col_legend = NULL
      ),
      marker_shape_list = list(
        marker_shape = NULL,
        marker_shape_opt = NULL,
        marker_shape_legend = "Grade"
      ),
      show_days_label = TRUE,
      xlim = c(-28, ADSL$max_day),
      xlab = "Study Day",
      title = paste("Patient Profile: ", ADSL$USUBJID)
    )

    expect_s3_class(p1, "ggplot")
  })

  it("works with tumr reponse domain", {
    ADRS <- osprey::rADRS |>
      dplyr::select(USUBJID, STUDYID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADTM)
    ADRS <- dplyr::left_join(ADSL, ADRS, by = c("USUBJID", "STUDYID"))
    p1 <- patient_domain_profile(
      domain = "Tumor Response (ADRS)",
      var_names = ADRS$PARAMCD,
      marker_pos = ADRS$ADY,
      arrow_end = ADSL$max_day,
      xtick_at = waiver(),
      line_col_list = NULL,
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0,
      marker_col_list = list(
        marker_col = factor(ADRS$AVALC),
        marker_col_opt = c(
          "CR" = "green", "PR" = "blue",
          "SD" = "yellow", "PD" = "red", "NE" = "pink",
          "Y" = "lightblue", "N" = "darkred"
        ),
        marker_col_legend = NULL
      ),
      marker_shape_list = list(
        marker_shape = factor(ADRS$AVALC),
        marker_shape_opt = c(
          "CR" = 21, "PR" = 24,
          "SD" = 23, "PD" = 22, "NE" = 14,
          "Y" = 11, "N" = 8
        ),
        marker_shape_legend = "Response"
      ),
      show_days_label = TRUE,
      xlim = c(-28, ADSL$max_day),
      xlab = "Study Day",
      title = paste("Patient Profile: ", ADSL$USUBJID)
    )
    p1

    expect_s3_class(p1, "ggplot")
  })

  it("works with concomitant medicine domain", {
    ADCM <- osprey::rADCM |>
      dplyr::select(USUBJID, STUDYID, ASTDTM, AENDTM, CMDECOD, ASTDY, AENDY)
    ADCM <- dplyr::left_join(ADSL, ADCM, by = c("USUBJID", "STUDYID"))
    p1 <- patient_domain_profile(
      domain = "Concomitant Med (ADCM)",
      var_names = ADCM$CMDECOD,
      marker_pos = ADCM[, c("ASTDY", "AENDY")],
      arrow_end = ADSL$max_day,
      xtick_at = waiver(),
      line_col_list = list(line_col_opt = "orange"),
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 50,
      marker_col_list = list(marker_col_opt = "orange"),
      marker_shape_list = NULL,
      show_days_label = TRUE,
      xlim = c(-28, ADSL$max_day),
      xlab = "Study Day",
      title = paste("Patient Profile: ", ADSL$USUBJID)
    )
    expect_s3_class(p1, "ggplot")
  })

  it("works with laboratory domain", {
    ADLB <- osprey::rADLB |>
      dplyr::select(
        USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE,
        ADTM, ADY, ATPTN, AVISITN, LBTESTCD, ANRIND
      )
    ADLB <- left_join(ADSL, ADLB, by = c("USUBJID", "STUDYID"))

    ADLB <- ADLB |>
      dplyr::group_by(USUBJID) |>
      dplyr::mutate(ANRIND = factor(ANRIND, levels = c("LOW", "NORMAL", "HIGH")))

    p1 <- patient_domain_profile(
      domain = "Laboratory (ADLB)",
      var_names = ADLB$LBTESTCD,
      marker_pos = ADLB$ADY,
      arrow_end = ADSL$max_day,
      xtick_at = waiver(),
      line_col_list = NULL,
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0,
      marker_col_list = list(
        marker_col = factor(ADLB$ANRIND),
        marker_col_opt = c(
          "HIGH" = "red", "LOW" = "blue",
          "NORMAL" = "green", "NA" = "green"
        )
      ),
      marker_shape_list = list(
        marker_shape = factor(ADLB$ANRIND),
        marker_shape_opt = c(
          "HIGH" = 24, "LOW" = 25,
          "NORMAL" = 23, "NA" = 23
        ),
        marker_shape_legend = "Labs Abnormality"
      ),
      show_days_label = TRUE,
      xlim = c(-30, ADSL$max_day),
      xlab = "Study Day",
      title = paste("Patient Profile: ", ADSL$USUBJID)
    )
    expect_s3_class(p1, "ggplot")
  })
})

describe("tests for g_patient_profile", {
  ADSL <- osprey::rADSL |>
    dplyr::filter(USUBJID == rADSL$USUBJID[1]) |>
    dplyr::mutate(
      TRTSDT = as.Date(TRTSDTM),
      max_date = max(as.Date(LSTALVDT), as.Date(DTHDT), na.rm = TRUE),
      max_day = as.numeric(as.Date(max_date) - as.Date(TRTSDT)) + 1
    ) |>
    dplyr::select(USUBJID, STUDYID, TRTSDT, max_day)


  # ADEX
  ADEX <- osprey::rADEX |>
    dplyr::select(USUBJID, STUDYID, ASTDTM, PARCAT2, AVAL, AVALU, PARAMCD)
  ADEX <- left_join(ADSL, ADEX, by = c("USUBJID", "STUDYID"))

  ADEX <- ADEX |>
    dplyr::filter(PARAMCD == "DOSE") |>
    dplyr::arrange(PARCAT2, PARAMCD) |>
    dplyr::mutate(diff = c(0, diff(AVAL, lag = 1))) |>
    dplyr::mutate(Modification = case_when(
      diff < 0 ~ "Decrease",
      diff > 0 ~ "Increase",
      diff == 0 ~ "None"
    )) |>
    dplyr::mutate(ASTDT_dur = as.numeric(
      as.Date(substr(as.character(ASTDTM), 1, 10)) -
        as.Date(TRTSDT) + 1
    ))

  # ADAE
  ADAE <- osprey::rADAE |>
    dplyr::select(USUBJID, STUDYID, AESOC, AEDECOD, AESER, AETOXGR, AEREL, ASTDY, AENDY)
  ADAE <- left_join(ADSL, ADAE, by = c("USUBJID", "STUDYID"))

  # ADRS
  ADRS <- osprey::rADRS |>
    dplyr::select(USUBJID, STUDYID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADTM)
  ADRS <- left_join(ADSL, ADRS, by = c("USUBJID", "STUDYID"))

  # ADCM
  ADCM <- osprey::rADCM |>
    dplyr::select(USUBJID, STUDYID, ASTDTM, AENDTM, CMDECOD, ASTDY, AENDY)
  ADCM <- dplyr::left_join(ADSL, ADCM, by = c("USUBJID", "STUDYID"))

  # ADLB
  ADLB <- osprey::rADLB |>
    dplyr::select(
      USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE, ADTM,
      ADY, ATPTN, AVISITN, LBTESTCD, ANRIND
    )
  ADLB <- dplyr::left_join(ADSL, ADLB, by = c("USUBJID", "STUDYID"))

  ADLB <- ADLB |>
    dplyr::group_by(USUBJID) |>
    dplyr::mutate(ANRIND = factor(ANRIND, levels = c("LOW", "NORMAL", "HIGH")))

  it("works with all domains", {
    g_patient_profile(
      ex = list(
        data = ADEX,
        var = ADEX$PARCAT2
      ),
      ae = list(
        data = ADAE,
        var = ADAE$AEDECOD,
        line_col = factor(ADAE$AESER),
        line_col_legend = "Serious",
        line_col_opt = c("Y" = "red", "N" = "blue")
      ),
      rs = list(
        data = ADRS,
        var = ADRS$PARAMCD
      ),
      cm = list(
        data = ADCM,
        var = ADCM$CMDECOD
      ),
      lb = list(
        data = ADLB,
        var = ADLB$LBTESTCD
      ),
      arrow_end_day = ADSL$max_day,
      xlim = c(-28, ADSL$max_day),
      xlab = "Study Day",
      title = paste("Patient Profile: ", ADSL$USUBJID)
    )
  })

  it("works with only ADEdomain", {
    g_patient_profile(
      ex = list(
        data = ADEX,
        var = ADEX$PARCAT2
      ),
      ae = list(
        data = ADAE,
        var = ADAE$AEDECOD,
        line_col = factor(ADAE$AESER),
        line_col_legend = "Serious",
        line_col_opt = c("Y" = "red", "N" = "blue")
      ),
      arrow_end_day = ADSL$max_day,
      xlim = c(-28, ADSL$max_day),
      xlab = "Study Day",
      title = paste("Patient Profile: ", ADSL$USUBJID)
    )
  })
})
