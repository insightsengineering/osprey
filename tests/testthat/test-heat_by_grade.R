describe("tests for g_heat_bygrade", {
  ADSL <- osprey::rADSL |> slice(1:30)
  ADEX <- osprey::rADEX |> filter(USUBJID %in% ADSL$USUBJID)
  ADAE <- osprey::rADAE |> filter(USUBJID %in% ADSL$USUBJID)
  ADCM <- osprey::rADCM |> filter(USUBJID %in% ADSL$USUBJID)
  # function to derive AVISIT from ADEX
  add_visit <- function(data_need_visit) {
    visit_dates <- ADEX |>
      dplyr::filter(PARAMCD == "DOSE") |>
      dplyr::distinct(USUBJID, AVISIT, ASTDTM) |>
      dplyr::group_by(USUBJID) |>
      dplyr::arrange(ASTDTM) |>
      dplyr::mutate(next_vis = lead(ASTDTM), is_last = ifelse(is.na(next_vis), TRUE, FALSE)) |>
      dplyr::rename(this_vis = ASTDTM)
    data_need_visit |>
      dplyr::select(USUBJID, ASTDTM) |>
      dplyr::left_join(visit_dates, by = "USUBJID", relationship = "many-to-many") |>
      dplyr::filter(ASTDTM > this_vis & (ASTDTM < next_vis | is_last == TRUE)) |>
      dplyr::left_join(data_need_visit, relationship = "many-to-many")
  }
  # add AVISIT in ADAE and ADCM
  ADAE <- add_visit(ADAE)
  ADCM <- add_visit(ADCM)
  exp_data <- ADEX |>
    dplyr::filter(PARCAT1 == "INDIVIDUAL") |>
    dplyr::group_by(USUBJID) |>
    # create a shorter subject identifier
    dplyr::mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1)) |>
    dplyr::mutate(ongo_var = (EOSSTT == "ONGOING")) |>
    dplyr::ungroup()
  anno_data <- ADSL |>
    dplyr::select(SEX, COUNTRY, USUBJID) |>
    dplyr::group_by(USUBJID) |>
    dplyr::mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1)) |>
    dplyr::ungroup() |>
    dplyr::select(-USUBJID)
  heat_data <- ADAE |>
    dplyr::select(USUBJID, AVISIT, AETOXGR) |>
    dplyr::group_by(USUBJID) |>
    dplyr::mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1)) |>
    dplyr::ungroup() |>
    dplyr::select(-USUBJID)
  heat_color_opt <- c(
    "No Event" = "gray90",
    "1" = "lightsteelblue1",
    "2" = "steelblue1",
    "3" = "steelblue4",
    "4" = "maroon",
    "5" = "brown4"
  )

  cmdecod_label <- attr(ADCM[["CMDECOD"]], "label")
  ADCM <- ADCM |>
    dplyr::filter(
      CMDECOD == "medname A_1/3" | CMDECOD == "medname A_2/3" | CMDECOD == "medname A_3/3"
    ) |>
    mutate(CMDECOD = factor(CMDECOD, levels = unique(CMDECOD)))
  attr(ADCM[["CMDECOD"]], "label") <- cmdecod_label
  conmed_data <- ADCM |>
    group_by(USUBJID) |>
    mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1))

  it("it works with concomitand medicine", {
    # example plotting conmed
    p1 <- g_heat_bygrade(
      id_var = "SUBJ",
      exp_data,
      visit_var = "AVISIT",
      ongo_var = "ongo_var",
      anno_data,
      anno_var = c("SEX", "COUNTRY"),
      heat_data,
      heat_color_var = "AETOXGR",
      heat_color_opt,
      conmed_data,
      conmed_var = "CMDECOD",
      conmed_color_opt = c("green", "green3", "green4")
    )
    expect_s3_class(p1, c("gtable", "gTree", "grob"))
  })

  it("it fails if data does not meet assumptions", {
    expect_error(g_heat_bygrade(
      id_var = "random",
      exp_data,
      visit_var = "AVISIT",
      ongo_var = "ongo_var",
      anno_data,
      anno_var = c("SEX", "COUNTRY"),
      heat_data,
      heat_color_var = "AETOXGR",
      heat_color_opt,
      conmed_data,
      conmed_var = "CMDECOD",
      conmed_color_opt = c("green", "green3", "green4")
    ), "must include a column named")
  })
})
