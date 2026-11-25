
describe("g_ae_sub produces the expected plot", {
  ADAE <- osprey::rADAE
  ADSL <- osprey::rADSL

  id <- ADAE$USUBJID
  arm <- ADAE$ACTARMCD
  arm_sl <- as.character(ADSL$ACTARMCD)
  subgroups_sl <- ADSL[, c("SEX", "RACE", "STRATA1")]
  subgroups <- ADAE[, c("SEX", "RACE", "STRATA1")]
  subgroups_levels <- list(
    RACE = list(
      "Total" = "Race",
      "AMERICAN INDIAN OR ALASKA NATIVE" = "American",
      "WHITE" = "White",
      "ASIAN" = "Asian",
      "BLACK OR AFRICAN AMERICAN" = "African"
    ),
    STRATA1 = list(
      "Total" = "Strata",
      "A" = "TypeA",
      "B" = "TypeB",
      "C" = "Typec"
    ),
    SEX = list(
      "Total" = "Sex",
      "M" = "Male",
      "F" = "Female"
    )
  )

  it("With arm_n = FALSE", {
    suppressWarnings({ # there is a warning due to usage of geom_errorbarh if ggplot
      p1 <- g_ae_sub(id,
        arm,
        arm_sl,
        subgroups,
        subgroups_sl,
        trt = "ARM A",
        ref = "ARM C",
        subgroups_levels = subgroups_levels,
        arm_n = FALSE
      )
    })
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("With arm_n = TRUE", {
    suppressWarnings({ # there is a warning due to usage of geom_errorbarh if ggplot
      p2 <- g_ae_sub(id,
        arm,
        arm_sl,
        subgroups,
        subgroups_sl,
        trt = "ARM A",
        ref = "ARM C",
        subgroups_levels = subgroups_levels,
        arm_n = TRUE
      )
    })
    expect_s3_class(p2, c("gtable", "gTree", "grob"))
  })
})
