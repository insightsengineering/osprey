source("https://raw.github.roche.com/NEST/nest_on_bee/master/bee_nest_utils.R")
bee_use_nest(release = "2021_07_07")

library(random.cdisc.data)
library(dplyr)
library(tidyr)

anrhi <- data.frame(ANRHI = c(50,50), PARAMCD = c("ALT","CRP"))

adsl <- radsl(N=30)

adlb <- radlb(adsl) %>%
  mutate(ANRHI = 50)
id <- adlb$USUBJID
term <- adlb$PARAMCD
aval <- adlb$AVAL
arm <- adlb$ARM
term_selected <- c("ALT","CRP")
anrhi <- adlb$ANRHI




g_hy_law <-function(id,
                    term,
                    aval,
                    arm,
                    term_selected,
                    anrhi,
                    folds = c(3,2),
                    text = c("Normal Range", "Hyperbilirubinemia", "Possible Hy's Law Range", "Temple's Corollary"),
                    caption = "Maximum values are those maximum values that occur post-baseline (no time constraints and not necessarily concurrent events).",
                    title = "Scatter Plot of Maximum Total Bilirubin versus Maximum Alanine Aminotransferase",
                    xlab = "Maximum Alanine Aminotransferase (/ULN)",
                    ylab = "Maximum Total Bilirubin (/ULN)"
                    ){

  anl <- data.frame(id, term, aval, arm, anrhi)

  anl <- anl %>%
    filter(term %in% term_selected) %>%
    group_by(id,term) %>%
    mutate(MAX = max(aval)) %>%
    slice(1) %>%
    mutate(ULN = MAX/anrhi) %>%
    pivot_wider(id_cols = c(id, arm), names_from = term, values_from = ULN)

p <- ggplot(data = anl) +

scale_x_continuous(
    name = xlab,
    breaks = log10(c(seq(0.1, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))),
    limits = c(-1, 2),
    labels = c(0.1, rep(" ", 8), 1, rep(" ", 8), 10, rep(" ", 8), 100),
    expand = c(0.01,0.01)
  ) +
  scale_y_continuous(
    name = ylab,
    breaks = log10(c(seq(0.1, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))),
    limits = c(-1, 2),
    labels = c(0.1, rep(" ", 8), 1, rep(" ", 8), 10, rep(" ", 8), 100),
    expand = c(0.01,0.01)
  ) +

  labs(title = title,
       caption = caption) +

  theme_bw(base_size = 14, base_family = "Arial") +

  theme(plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        panel.grid = element_blank()) +

  geom_segment(
    aes(x = log10(folds[1]), y = log10(0), xend = log10(folds[1]), yend = log10(75)),
    size = 0.25,
    color = "grey"
  ) +
  geom_segment(
    aes(x = log10(0), y = log10(folds[2]), xend = log10(50), yend = log10(folds[2])),
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
  annotate("text", label = paste0(folds[1],"XULN"), x = log10(folds[1]), y = log10(90)) +
  annotate("text", label = paste0(folds[2],"XULN"), x = log10(75), y = log10(folds[2])) +
  annotate("text", label = text[1], x = log10(0.2), y = log10(0.12)) +
  annotate("text", label = text[2], x = log10(0.2), y = log10(80)) +
  annotate("text", label = text[3], x = log10(40), y = log10(80)) +
  annotate("text", label = text[4], x = log10(40), y = log10(0.12)) +
  geom_point(aes(x = log10(.data[[term_selected[1]]]), y = log10(.data[[term_selected[2]]]), shape=arm, color = arm)) +
  scale_shape_manual(values = c(1:length(unique(arm))))

g <- ggplotGrob(p)

grid.newpage()
grid.draw(g)
invisible(g)
}
