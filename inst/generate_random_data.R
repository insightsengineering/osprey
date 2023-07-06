rADSL <- scda::synthetic_cdisc_data("rcd_2023_03_17")$adsl # nolint
rADTTE <- scda::synthetic_cdisc_data("rcd_2023_03_17")$adtte # nolint
rADRS <- scda::synthetic_cdisc_data("rcd_2023_03_17")$adrs # nolint
rADEX <- scda::synthetic_cdisc_data("rcd_2023_03_17")$adex # nolint
rADAE <- scda::synthetic_cdisc_data("rcd_2023_03_17")$adae # nolint
rADTR <- scda::synthetic_cdisc_data("rcd_2023_03_17")$adtr # nolint
rADCM <- scda::synthetic_cdisc_data("rcd_2023_03_17")$adcm # nolint
rADLB <- scda::synthetic_cdisc_data("rcd_2023_03_17")$adlb # nolint

usethis::use_data(rADSL)
usethis::use_data(rADTTE)
usethis::use_data(rADRS)
usethis::use_data(rADEX)
usethis::use_data(rADAE)
usethis::use_data(rADTR)
usethis::use_data(rADCM)
usethis::use_data(rADLB)
