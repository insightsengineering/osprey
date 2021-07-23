source("https://raw.github.roche.com/NEST/nest_on_bee/master/bee_nest_utils.R")
bee_use_nest(release = "2021_07_07")
devtools::load_all()

library(random.cdisc.data)
library(dplyr)
library(tidyr)

anrhi <- data.frame(ANRHI = c(50,50), PARAMCD = c("ALT","CRP"))

adsl <- radsl(N=30)
adlb <- radlb(adsl) %>%
  filter(AVISITN>0 & PARAMCD %in% c("CRP","ALT")) %>%
  group_by(USUBJID,PARAMCD) %>%
  mutate(MAX = max(AVAL)) %>%
  slice(1) %>%
  left_join(anrhi) %>%
  mutate(ULN = MAX/ANRHI) %>%
  pivot_wider(id_cols = USUBJID, names_from = PARAMCD, values_from = ULN)
