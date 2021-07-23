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


ggplot(data = adlb) +

  scale_x_continuous(
    name = "Maximum Alanine Aminotransferase (/ULN)",
    breaks = log10(c(seq(0.1, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))),
    limits = c(-1, 2),
    labels = c(0.1, rep(" ", 8), 1, rep(" ", 8), 10, rep(" ", 8), 100),
    expand = c(0.01,0.01)
  ) +
  scale_y_continuous(
    name = "Maximum Total Bilirubin (/ULN)",
    breaks = log10(c(seq(0.1, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))),
    limits = c(-1, 2),
    labels = c(0.1, rep(" ", 8), 1, rep(" ", 8), 10, rep(" ", 8), 100),
    expand = c(0.01,0.01)
  ) +

  labs(title = "Scatter Plot of Maximum Total Bilirubin versus Maximum Alanine Aminotransferase",
       caption = "Maximum values are those maximum values that occur post-baseline (no time constraints and not necessarily concurrent events).") +

  theme_bw(base_size = 14, base_family = "Arial") +

  theme(plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        panel.grid = element_blank())

