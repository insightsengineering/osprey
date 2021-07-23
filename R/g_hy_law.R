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

p <- ggplot(data = adlb) +
  geom_point(aes(x = log(ALT), y = log(CRP))) +
    scale_x_continuous(
    name = "Peak ALT, xULN",
    breaks = log10(c(seq(0.1, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))),
    limits = c(-1, 2),
    labels = c(0.1, rep(" ", 8), 1, rep(" ", 1), 3, rep(" ", 6), 10, rep(" ", 8), 100)
  ) +
  scale_y_continuous(
    name = "Peak TBILI, xULN",
    breaks = log10(c(seq(0.1, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))),
    limits = c(-1, 2),
    labels = c(0.1, rep(" ", 8), 1, 2, rep(" ", 7), 10, rep(" ", 8), 100)
  ) +
  geom_segment(
    aes(x = log10(3), y = log10(0), xend = log10(3), yend = log10(85)),
    size = 0.25,
    color = "grey"
  ) +
  geom_segment(
    aes(x = log10(0), y = log10(2), xend = log10(80), yend = log10(2)),
    size = 0.25,
    color = "grey"
  ) +
  geom_segment(
    aes(x = log10(1), y = log10(0), xend = log10(1), yend = log10(1)),
    size = 0.25,
    color = "black"
  ) +
  geom_segment(
    aes(x = log10(0), y = log10(1), xend = log10(1), yend = log10(1)),
    size = 0.25,
    color = "black"
  ) +
  annotate("text", label = "3XULN", x = log10(3), y = log10(100)) +
  annotate("text", label = "2XULN", x = log10(100), y = log10(2)) +
  annotate("text", label = "Hyperbilirubinemia", x = log10(0.2), y = log10(80)) +
  annotate("text", label = "Possible Hy's Law Range", x = log10(40), y = log10(80)) +
  annotate("text", label = "Normal Range", x = log10(0.2), y = log10(0.1)) +
  annotate("text", label = "Temple's Corollary", x = log10(40), y = log10(0.1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



