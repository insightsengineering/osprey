describe("tests for g_events_term_id", {
  ADSL <- osprey::rADSL
  ADAE <- osprey::rADAE

  ADAE <- ADAE |>
    mutate(AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X")) |>
    mutate(AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo"))
  attr(ADAE[["AEREL1"]], "label") <- "AE related to A: Drug X"
  attr(ADAE[["AEREL2"]], "label") <- "AE related to B: Placebo"

  term <- ADAE$AEDECOD
  id <- ADAE$USUBJID
  arm <- ADAE$ACTARMCD
  arm_N <- table(ADSL$ACTARMCD)
  ref <- "ARM A"
  trt <- "ARM C"

  it("works with standard parameters", {
    suppressWarnings({
      p1 <- g_events_term_id(
        term,
        id,
        arm,
        arm_N
      )
    })

    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with sorting by riskdiff", {
    p1 <- g_events_term_id(
      term,
      id,
      arm,
      arm_N,
      trt = trt,
      ref = ref,
      sort_by = "riskdiff",
      diff_ci_method = "ac",
      conf_level = 0.9
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("works with sorting by meanrisk", {
    p1 <- g_events_term_id(
      term,
      id,
      arm,
      arm_N,
      sort_by = "meanrisk",
      axis_side = "right",
      fontsize = 5
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })
})

describe("tests for create flags var", {
  it("works with default arguments", {
    ADAE <- osprey::rADAE

    ADAE <- ADAE |>
      mutate(AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X")) |>
      mutate(AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo"))
    attr(ADAE[["AEREL1"]], "label") <- "AE related to A: Drug X"
    attr(ADAE[["AEREL2"]], "label") <- "AE related to B: Placebo"

    flags_var <- create_flag_vars(ADAE)

    expect_s3_class(flags_var, "data.frame")
  })
})