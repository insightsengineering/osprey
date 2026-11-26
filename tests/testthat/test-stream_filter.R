test_that("test for stream_filter", {
  ADSL <- osprey::rADSL
  ADTTE <- osprey::rADTTE
  filters <- as.data.frame(rbind(
    c(ID = "IT", FLTTARGET = "SLREF", FLTWHERE = "where 1 eq 1"),
    c(ID = "BIO", FLTTARGET = "SLREF", FLTWHERE = "where BMRKR1 ge 4.3"),
    c(ID = "M", FLTTARGET = "SLREF", FLTWHERE = "where SEX eq 'M'"),
    c(ID = "PFS", FLTTARGET = "ANL", FLTWHERE = "where PARAMCD eq 'PFS'"),
    c(ID = "OS", FLTTARGET = "ANL", FLTWHERE = "where PARAMCD eq 'OS'")
  ))

  expect_output(stream_filter(
    slref = ADSL,
    anl = ADTTE,
    suffix = "IT_PFS_BIO",
    filters = filters
  ))
})

test_that("test for stream_filter_index", {
  AEACN <- c("DRUG MODIFIED", "DRUG STOPPED", "DOSE/DRUG MODIFIED")
  index <- stream_filter_index(AEACN, "DRUG MODIFIED")
  expect_true(is.logical(index))
  expect_identical(length(index), length(AEACN))
})

test_that("test for stream_filter_convwhere", {
  filter_convwhere <- stream_filter_convwhere(x = "where X in (1 2 3 4) and Y gt 4 ")
  expect_true(grepl("%in%", filter_convwhere))
})
