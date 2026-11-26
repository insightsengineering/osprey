test_that("test for spiderplot simple", {
  ADSL <- osprey::rADSL[1:15, ]
  ADTR <- osprey::rADTR
  ANL <- left_join(ADSL, ADTR)

  p1 <- ANL |>
    dplyr::filter(ANL01FL == "Y" & PARAMCD == "SLDINV") |>
    spiderplot_simple(group_col = "SEX", days = "ADY", mes_value = "AVAL")
  expect_s3_class(p1, "ggplot")
})
