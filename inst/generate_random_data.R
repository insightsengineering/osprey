## Generate Random Data ----
##
## First Draft: Federico Mattiello & Adrian Waddell
##
## Modified by Chendi Liao (liaoc10) for osprey ED teal module development
## - Random VAD data generation ED study VADS

library(dplyr)

#Function to sample random data for factor variables
sample_fct <- function(x, N, ...) { # nolint
  factor(sample(x, N, replace = TRUE, ...), levels = x)
}

#Function to sample random data for factor variables when if condition is met
ifgen_fct <- function(t, tx, tp = NULL, fx, fp = NULL) {
 factor(ifelse(t, sample(tx, length(t), replace = TRUE, prob = tp),
                  sample(fx, length(t), replace = TRUE, prob = fp)),
        levels = union(tx, fx))
}

#Function to sample random data for factor variables
sample_char <- function(x, N, ...) { # nolint
  sample(x, N, replace = TRUE, ...)
}

#Function to sample random data for factor variables when if condition is met
ifgen_char <- function(t, tx, tp = NULL, fx, fp = NULL) {
  ifelse(t, sample(tx, length(t), replace = TRUE, prob = tp),
           sample(fx, length(t), replace = TRUE, prob = fp))
}

#' Sets column labels of a `data.frame`.
`var_labels<-` <- function(x, value) { # nolint
  checkmate::assert_data_frame(x)
  checkmate::assert_character(value, len = ncol(x))

  for (i in seq_along(x)) {
    if (!is.na(value[i])) {
      attr(x[[i]], "label") <- value[i]
    }
  }

  x
}

#####------ ADSL - Subject Level Analysis Dataset -------#####
# Assuming study design: 3 arms, 12 cycles Q1W, EoT 4 weeks after last cycle

set.seed(12345)

N <- 50 # nolint

ADSL <- tibble( # nolint
  SUBJID  = paste("id", sprintf(paste0("%0", nchar(N), "d"), seq_len(N)), sep = "-"),
  STUDYID = rep("AB12345", N),
  SITEID  = paste0("XYZ", 1:3) %>% sample_fct(N),
  USUBJID = paste(STUDYID, SITEID, SUBJID, sep = "-"),
  AGE     = sapply(floor(rnorm(N, mean = 20, sd = 20)), max, 0) + 20,
  SEX     = c("F", "M", "U", "UNDIFFERENTIATED") %>% sample_fct(N, prob = c(.5, .48, .015, .005)),
  ARMCD   = c("ARM A", "ARM B", "ARM C") %>% sample_fct(N),
  COUNTRY = c("FRA", "DNK", "ESP", "AUS") %>% sample_fct(N),
  RACE    = c("WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
              "AMERICAN INDIAN OR ALASKA NATIVE", "UNKNOWN") %>% sample_fct(N, prob = c(.4, .2, .2, .05, .05, .1)),
  STRATA1 = c("STRATA A", "STRATA B", "STRATA C") %>% sample_fct(N),
  STRATA2 = c("STRATA 1", "STRATA 2") %>% sample_fct(N),
  BMK1    = rchisq(N, 6),
  BMK2    = c("Low", "Medium", "High") %>% sample_fct(N),
  SAFFL   = c("Y", "N") %>% sample_fct(N, prob = c(.9, .1)),
  EOSSTT  = c("Completed", "Discontinued", "Ongoing") %>% sample_fct(N, prob = c(.3, .3, .4))
) %>%
  dplyr::mutate(
    ARM         = recode(ARMCD, "ARM A" = "A: Placebo", "ARM B" = "B: Drug X", "ARM C" = "C: Combination"),
    ACTARM      = ARM,
    ACTARMCD    = ARMCD,
    AGEGR1      = ifelse(AGE < 65, "<65", ">=65"),
    TRTDURD     = ifelse(EOSSTT == "Completed",
                         84 + floor(runif(N, min = 0, max = 10)),
                         floor(runif(N, min = 2, max = 83))),
    EOSDY       = ifelse(EOSSTT == "Discontinued", TRTDURD,
                         ifelse(EOSSTT == "Completed", TRTDURD + floor(runif(N, min = 28, max = 36)), NA)),
    DCSREAS     = ifgen_char(EOSSTT == "Discontinued",
                             tx = c("Adverse Event", "Death", "Progressive Disease",
                                    "Symptomatic Deterioation", "Withdrawal by Subject", "Physician Decision"),
                             tp = c(.2, .2, .2, .2, .1, .1), fx = ""),
    DCSREASP    = ifelse(DCSREAS != "", paste(DCSREAS, "Reason", sample(1:5, N, replace = TRUE)), ""),
    DCSREAS_GRP = ifelse(DCSREAS == "", "", ifelse(DCSREAS %in% c("Adverse Event", "Death"), "Safety", "Non-Safety")),
    DTHFL       = ifelse(DCSREAS == "Death", "Y",
                         ifgen_char(EOSSTT != "Ongoing", tx = c("Y", "N"), tp = c(.2, .8), fx = "N")),
    DTHCAUS     = ifgen_char(DTHFL == "Y",
                             tx = c("Progressive Disease", "Adverse Event", "Other Medical Conditions"),
                             tp = c(0.5, 0.2, 0.3),
                             fx = ""),
    AEWITHFL    = ifelse(DCSREAS == "Adverse Event" | DTHCAUS == "Adverse Event", "Y", "N")
)


#####------ ADAE - Adverse Event Analysis Dataset ------#####

#meddra <- haven::read_sas("C:/Users/liaoc10/Desktop/meddra_hierarchy.sas7bdat") # nolint
meddra <- read_bce("/opt/BIOSTAT/prod/acp/libraries/meddra_hierarchy.sas7bdat")

meddra <- meddra %>%
  dplyr::filter(MEDDRA_VERSION == "21.0") %>%
  select(-MEDDRA_VERSION, -PRIMARY_PATH) %>%
  dplyr::rename(AEDECOD = PT_NAME, AEPTCD = PT_CODE,
                AESOC = SOC_NAME, AESOCCD = SOC_CODE,
                AEHLGT = HLGT_NAME, AEHLGTCD = HLGT_CODE,
                AEHLT = HLT_NAME, AEHLTCD = HLT_CODE) %>%
  dplyr::mutate(AEBODSYS = AESOC,
         AEBDSYCD = AESOCCD,
         AETERM   = toupper(AEDECOD))

ADAE <- split(ADSL, ADSL$USUBJID) %>% lapply(FUN = function(pinfo) { # nolint
  nae <- sample(1:10, 1)

  AE <- cbind( # nolint
    pinfo,
    meddra[sample(seq_len(nrow(meddra)), nae), ],
    tibble(
      AESEQ   = seq_len(nae),
      AEREL   = c("Y", "N") %>% sample_char(nae, prob = c(.2, .8)),
      AESER   = c("Y", "N") %>% sample_char(nae, prob = c(.2, .8)),
      AEACN   = c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE NOT CHANGED",
                  "DOSE REDUCED", "UNKNOWN", "NOT APPLICABLE") %>%
        sample_char(nae, prob = c(.18, .02, .5, .1, .05, .15)),
      AEOUT   = c("NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE",
                  "RECOVERING/RESOLVING", "RECOVERED/RESOLVED", "UNKNOWN") %>%
        sample_char(nae, prob = c(.2, .1, .1, .5, .1)),
      AETOXGR = paste(1:4) %>% sample_char(nae, prob =  c(.4, .3, .2, .1))
    )
  ) %>%
    dplyr::mutate(
      AESDTH  = "N",
      TRTEMFL = "Y",
      AREL    = AEREL,
      ATOXGR  = AETOXGR
    )

  # Edit last AE record if AE lead to withdrawl according to ADSL$AEWITHFL
  if (pinfo$AEWITHFL == "Y") {
    AE <- AE %>% dplyr::mutate( # nolint
      AESER = ifelse(AESEQ == nae, "Y", AESER),
      AEACN = ifelse(AESEQ == nae, "DRUG WITHDRAWN", AEACN)
    )
  }

  # Edit last AE record if AE lead to death according to ADSL$DTHCAUS
  if (pinfo$DTHCAUS == "Adverse Event") {
    AE <- AE %>% dplyr::mutate( # nolint
      AEOUT   = ifelse(AESEQ == nae, "FATAL", AEOUT),
      AESER   = ifelse(AESEQ == nae, "Y", AESER),
      AETOXGR = ifelse(AESEQ == nae, "5", AETOXGR),
      ATOXGR  = ifelse(AESEQ == nae, "5", ATOXGR),
      AESDTH  = ifelse(AESEQ == nae, "Y", AESDTH)
    )
  }

  AE

})  %>%
  Reduce(rbind, .)


#####----- ADTTE - Time-to-Event Analysis Dataset -----######

lookup_ADTTE <- tribble( # nolint
  ~ARMCD,  ~PARAMCD, ~PARAM, ~LAMBDA, ~CNSR_P, # nolint
  "ARM A", "OS",   "Overall Survival",          1 / 100,  0.2,
  "ARM B", "OS",   "Overall Survival",          1 / 80,   0.4,
  "ARM C", "OS",   "Overall Survival",          1 / 60,   0.42,
  "ARM A", "PFS",  "Progression Free Survival", 1 / 150,  0.1,
  "ARM B", "PFS",  "Progression Free Survival", 1 / 100,  0.3,
  "ARM C", "PFS",  "Progression Free Survival", 1 / 80,   0.32,
  "ARM A", "EFS",  "Event Free Survival",       1 / 100,  0.08,
  "ARM B", "EFS",  "Event Free Survival",       1 / 80,   0.2,
  "ARM C", "EFS",  "Event Free Survival",       1 / 60,   0.23
)

evntdescr_sel <- c(
  "Death",
  "Disease Progression",
  "Last Tumor Assessment",
  "Adverse Event",
  "Last Date Known To Be Alive"
)

ADTTE <- split(ADSL, ADSL$USUBJID) %>% lapply(FUN = function(pinfo) { # nolint

  lookup_ADTTE %>%
    dplyr::filter(ARMCD == as.character(pinfo$ACTARMCD)) %>%
    rowwise() %>%
    dplyr::mutate(CNSR = sample(c(0, 1), 1, prob = c(1 - CNSR_P, CNSR_P)),
           AVAL = rexp(1, LAMBDA),
           AVALU = "DAYS",
           EVNTDESC = if (CNSR == 1) sample(evntdescr_sel[-c(1:2)], 1) else sample(evntdescr_sel, 1)) %>%
    select(-ARMCD, -LAMBDA, -CNSR_P) %>%
    cbind(pinfo, .)

})  %>%
  Reduce(rbind, .)

#Clean up
rm(evntdescr_sel, lookup_ADTTE)


#####------ ADRS - Tumor Response Analysis Dataset ------#####

# PARAM, PARAMCD, AVAL, AVALC, AVISIT, AVISITN
param_codes <- setNames(1:5, c("CR", "PR", "SD", "PD", "NE"))

lookup_ADRS <- expand.grid( # nolint
  ARMCD = c("ARM A", "ARM B", "ARM C"),
  AVALC = names(param_codes)
) %>% dplyr::mutate(
  AVAL = param_codes[AVALC],
  p_c6  = c(c(.25, .2, .45), c(.25, .3, .3), c(.4, .3, .15), c(.1, .2, .05), c(0, 0, .05)),
  p_c12 = c(c(.2, .4, .7), c(.2, .3, .2), c(.4, .15, .03), c(.2, .14, .05), c(.1, .01, .02)),
  p_eot = c(c(.2, .3, .5), c(.1, .2, .3), c(.2, .2, .1), c(.5, .3, .1), rep(0, 3))
)

ADRS <- split(ADSL, ADSL$USUBJID) %>% lapply(FUN = function(pinfo) { # nolint
  probs <- lookup_ADRS %>%
    dplyr::filter(ARMCD == as.character(pinfo$ACTARMCD))

  # 1st response assessment
  ady_c6 <- 42 + floor(runif(1, min = 0, max = 5))
  rsp_c6 <- sample(probs$AVALC, 1, prob = probs$p_c6) %>% as.character()

  # 2nd response assessment
  ady_c12 <- ady_c6 + 42 + floor(runif(1, min = 0, max = 5))
  rsp_c12 <- sample(probs$AVALC, 1, prob = probs$p_c12) %>% as.character()

  # last response assessment
  ady_eot <- ady_c12 + floor(runif(1, min = 28, max = 36))
  rsp_eot <- sample(probs$AVALC, 1, prob = probs$p_eot) %>% as.character()

  #best overall responses
  best_rsp <- min(param_codes[c(rsp_c6, rsp_c12, rsp_eot)])
  best_rsp_i <- which.min(param_codes[c(rsp_c6, rsp_c12, rsp_eot)])
  obj_rsp <- ifelse(names(param_codes)[best_rsp] %in% c("CR", "PR"), "Y", "N")

  avisit <- c("Cycle 6 Day 1", "Cycle 12 Day 1", "End of Treatment")
  ady <- c(ady_c6, ady_c12, ady_eot)

  tibble(
    PARAMCD = c(rep("OVRINV", 3), "BESRSPI", "OBJRSPI", "LSTASDI"),
    PARAM = recode(PARAMCD, OVRINV = "Overall Response by Investigator",
                     BESRSPI = "Best Overall Response by Investigator",
                     OBJRSPI  = "Objective Responders by Investigator",
                     LSTASDI = "Last Tumor Assessment by Investigator"),
    AVALC = c(rsp_c6, rsp_c12, rsp_eot,
                names(param_codes)[best_rsp],
                obj_rsp,
                rsp_eot),
    AVAL = ifelse(PARAMCD != "OBJRSPI", param_codes[AVALC],
                    ifelse(AVALC == "Y", 1, 0)),
    AVISIT  = factor(c(avisit, rep(avisit[best_rsp_i], 2), avisit[3]), levels = avisit),
    AVISITN = as.numeric(AVISIT),
    ADY     = c(ady, rep(ady[best_rsp_i], 2), ady[3])
  ) %>% cbind(pinfo, .)
}) %>%
  Reduce(rbind, .)

#Clean up
rm(param_codes, lookup_ADRS)

#####--------- ADTR - Tumor Burden Analysis Dataset -------#####
# PARAM, PARAMCD, AVAL, AVALC, AVISIT, AVISITN

lookup_ADTR <- expand.grid( # nolint
  ARMCD  = c("ARM A", "ARM B", "ARM C"),
  type = c("mean", "sd")
) %>% dplyr::mutate(
  aval_scr = c(c(80, 80, 80), c(20, 22, 25)),
  pchg_c6  = c(c(20, 0, -10), c(20, 20, 20)),
  pchg_c12 = c(c(40, -10, -20), c(20, 25, 30)),
  pchg_eot = c(c(30, 0, -30), c(20, 30, 30))
)

ADTR <- split(ADSL, ADSL$USUBJID) %>% lapply(FUN = function(pinfo) { # nolint

  probs <- lookup_ADTR %>%
    dplyr::filter(ARMCD == as.character(pinfo$ACTARMCD))

  # screening
  aval_scr <- do.call(rnorm, as.list(c(1, probs$aval_scr)))

  # C6D1
  pchg_c6 <- do.call(rnorm, as.list(c(1, probs$pchg_c6)))
  ady_c6 <- 42 + floor(runif(1, min = 0, max = 5))

  # C12D1
  pchg_c12 <- do.call(rnorm, as.list(c(1, probs$pchg_c12)))
  ady_c12 <- ady_c6 + 42 + floor(runif(1, min = 0, max = 5))

  # end of treatment
  pchg_eot <- do.call(rnorm, as.list(c(1, probs$pchg_eot)))
  ady_eot  <- ady_c12 + floor(runif(1, min = 28, max = 36))

  avisit <- c("Screening", "Cycle 6 Day 1", "Cycle 12 Day 1", "End of Treatment")

  tr <- tibble(
    PARAMCD = "SLDINV",
    PARAM   = recode(PARAMCD, SLDINV = "Sum of Longest Diameter by Investigator"),
    AVISIT  = factor(avisit, levels = avisit),
    AVISITN = c(1:4),
    ADY     = c(-1, ady_c6, ady_c12, ady_eot),
    AVALU   = rep("mm", 4),
    BASE    = rep(aval_scr, 4),
    PCHG    = c(NA, pchg_c6, pchg_c12, pchg_eot),
    ABLFL   = c("Y", rep("", 3)),
    ONTRTFL = c("", rep("Y", 3)),
    ANL01FL = rep("Y", 4),
    ANL02FL = c("Y", "", "", "Y"),
    ANL03FL = c("Y", rep("", 3)),
    DTYPE   = rep("", 4)
  ) %>% dplyr::mutate(
    CHG     = BASE * (PCHG / 100),
    AVAL    = BASE + CHG,
    AVALC   = as.character(AVAL)
  )

  tr_min <- tr %>%
    slice(which.min(AVAL)) %>%
    dplyr::mutate(AVISIT  = "POST-BASELINE MINIMUM",
           ANL01FL = "",
           ANL02FL = "",
           ANL03FL = "Y",
           DTYPE   = "MINIMUM")

  rbind(tr, tr_min) %>% cbind(pinfo, .)

}) %>%
  Reduce(rbind, .)


#Clean up
rm(lookup_ADTR)


#############################################################################
#Function to carry over the same labels from source datasets to target datasets
match_label <- function(target, source) {
  if (!is(source, "data.frame")) stop("source must be a data.frame")
  if (!is(target, "data.frame")) stop("target must be a data.frame")

  source_names <- names(source)
  target_names <- names(target)
  if (is.null(source_names)) stop("source dataset does not have any variables")

  map_varnames <- match(target_names, source_names)

  if (sum(map_varnames, na.rm = TRUE) < 1) stop("no matching variable name between source and target")

  for (i in seq_along(map_varnames)) {
    attr(target[[i]], "label") <- attr(source[[map_varnames[[i]]]], "label")
  }

  target
}


#Attach labels to variables
rADSL <- ADSL # nolint
column_labels <- list(
  STUDYID = "Study Identifier",
  SUBJID = "Subject Identifier for the Study",
  USUBJID = "Unique Subject Identifier",
  SITEID = "Study Site Identifier",
  AGE = "Age",
  AGEGR1 = "Age Group 1",
  SEX = "Sex",
  RACE = "Race",
  ARMCD = "Planned Arm Code",
  ARM = "Description of Planned Arm",
  ACTARMCD = "Actual Arm Code",
  ACTARM = "Description of Actual Arm",
  COUNTRY = "Country",
  BMK1 = "Cont. Biomarker 1",
  BMK2 = "Cat. Biomarker 2",
  STRATA1 = "Stratification Factor 1",
  STRATA2 = "Stratification Factor 2",
  SAFFL = "Safety Evaluable Population Flag",
  TRTDURD = "Total Treatment Duration (Days)",
  EOSSTT = "End of Study Status",
  EOSDY = "End of Study Relative Day",
  DCSREAS = "Reason for Discontinuation from Study",
  DCSREAS_GRP = "Grouped Reason for Disc. from Study",
  DTHFL = "Subject Death Flag",
  DTHCAUS = "Cause of Death",
  AEWITHFL = "AE Leading to Drug Withdrawal Flag"
)
var_labels(rADSL)[names(column_labels)] <- as.character(column_labels)

rADAE <- ADAE %>% # nolint
  match_label(rADSL)
column_labels <- list(
  AEDECOD  = "Dictionary Derived Term",
  AEPTCD   = "Dictionary Derived Term Code",
  AESOC    = "Primary System Organ Class",
  AESOCCD  = "Primary System Organ Class Code",
  AEHLGT   = "High Level Group Term",
  AEHLGTCD = "High Level Group Term Code",
  AEHLT    = "High Level Term",
  AEHLTCD  = "High Level Term Code",
  AEBODSYS = "Body System or Organ Class",
  AEBDSYCD = "Body System or Organ Class Code",
  AETERM   = "Reported Term for the Adverse Event",
  AESEQ    = "Sequence Number",
  AEREL    = "Causality",
  AESER    = "Serious Event",
  AEACN    = "Action Taken with Study Treatment",
  AEOUT    = "Outcome of AE",
  AETOXGR  = "Standard Toxicity Grade",
  AESDTH   = "Results in Death",
  TRTEMFL  = "Treatment Emergent Analysis Flag",
  AREL     = "Analysis Causality",
  ATOXGR   = "Analysis Toxicity Grade"
)
var_labels(rADAE)[names(column_labels)] <- as.character(column_labels)

rADTR <- ADTR %>% # nolint
  match_label(rADSL)
column_labels <- list(
  PARAM   = "Parameter Description",
  PARAMCD = "Parameter Code",
  AVAL    = "Analysis Value",
  AVALC   = "Analysis Value (C)",
  AVALU   = "Analysis Value Unit",
  AVISIT  = "Analysis Visit",
  AVISITN = "Analysis Visit (N)",
  ADY     = "Analysis Relative Day",
  BASE    = "Baseline Value",
  CHG     = "Change from Baseline",
  PCHG    = "Percent Change from Baseline",
  ABLFL   = "Baseline Record Flag",
  ONTRTFL = "On Treatment Flag",
  ANL01FL = "Analysis Flag 01 Baseline Post-Baseline",
  ANL02FL = "Analysis Flag 02 Last Obs Within BType",
  ANL03FL = "Analysis Flag 03 Min Obs Within BType",
  DTYPE   = "Derivation Type"
)
var_labels(rADTR)[names(column_labels)] <- as.character(column_labels)

rADTTE <- ADTTE %>% # nolint
  match_label(rADTR)
column_labels <- list(EVNTDESC = "Event Description", CNSR = "Censoring Status Value(1=cens, 0=evt)")
var_labels(rADTTE)[names(column_labels)] <- as.character(column_labels)

rADRS <- ADRS %>% # nolint
  match_label(rADTR)

rm(ADSL, ADAE, ADRS, ADTTE, ADTR)

#Exporting dummy data
devtools::use_data(meddra)
devtools::use_data(rADSL)
devtools::use_data(rADAE)
devtools::use_data(rADTTE)
devtools::use_data(rADRS)
devtools::use_data(rADTR)
