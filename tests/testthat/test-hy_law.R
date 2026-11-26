describe("tests for g_hy_law", {
  it("works with default arguments", {
    adsl <- osprey::rADSL
    adlb <- osprey::rADLB |>
      dplyr::mutate(ANRHI = 50)

    p1 <- g_hy_law(
      id = adlb$USUBJID,
      term = adlb$PARAMCD,
      aval = adlb$AVAL,
      arm = adlb$ARM,
      term_selected = c("ALT", "CRP"),
      anrhi = adlb$ANRHI,
      folds = c(3, 2),
      text = c("Normal Range", "Hyperbilirubinemia", "Possible Hy's Law Range", "Temple's Corollary"),
      caption = paste(
        "Maximum values are those maximum values that occur",
        "post-baseline (no time constraints and not necessarily concurrent events)."
      ),
      title = "Max. Total Bilirubin vs. Max. Alanine Aminotransferase",
      xlab = "Maximum Alanine Aminotransferase (/ULN)",
      ylab = "Maximum Total Bilirubin (/ULN)"
    )

    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })
})
