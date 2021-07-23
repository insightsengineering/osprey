#' Hy's Law Plot
#'
#' This graphic is based upon the eDISH (evaluation of Drug Induced Serious Hepatotoxicity)
#' plot of Watkins et al in a 2008 publication from Hepatology. Maximum total bilirubin values
#' (TBL) are plotted versus maximum alanine transaminase (ALT) values. Maximum values are
#' defined as the maximum post-baseline value at any time during the entire length of the
#' observation period. Both axes are in log scale to control for the dispersion of the data.
#' The values are plotted in ‘times upper limit of normal’ where a value of 1 would mean that
#' the result was normal. Any value above or below 1 would be considered above the upper limit
#' or normal or below the upper limit of normal respectively. For instance, a value of 3 would
#' be read as ‘3 times the upper limit of normal’. Reference lines are included to determine
#' various states, based upon clinical interpretation of the values and includes the following:
#'
#' Hyperbilirubinemia TBL at least 2 xULN and ALT less than 3 xULN
#' Normal Range TBL <= 1 xULN and ALT <= 1xULN
#' Temple’s Corollary TBL <= 1 xULN and ALT at least 3 xULN
#' Possible Hy’s Law TBL at least 2 xULN and ALT at least 3 xULN
#'
#' This plot can easily be adjusted for other lab parameters and reference ranges as needed.
#' Consultation with a clinical expert to determine which associations would be clinically
#' meaningful and how to interpret those associations is recommended.
#'
#' @param id unique subject identifier variable. Usually it is USUBJID
#' @param term the term of the observation. Usually it is PARAMCD
#' @param aval the value corresponding to the observation
#' @param arm the treatment ARM corresponding to the observation
#' @param term_selected string vector of length 2 - the two terms selected to be drawn. First value
#' corresponds to that plotted on the x-axis. Second value corresponds to that plotted on the y-axis
#' @param anrhi the high limit of normal range.
#' @param folds numeric vector of length 2 - the position of the lines to be drawn.
#' Default = c(3,2) which corresponds to a line at position 3 on the x-axis and 2 on the y-axis.
#' @param text string vector of length 4 - the text to be shown on four squares.
#' First value corresponds to that shown in the bottom left quadrant. subsequent values move through
#' the graph clockwise.
#' Default = c("Normal Range", "Hyperbilirubinemia", "Possible Hy's Law Range", "Temple's Corollary")
#' @param caption string of text for footnote. Details of methodology can be shown here.
#' Default = "Maximum values are those maximum values that occur post-baseline (no time constraints
#' and not necessarily concurrent events)."
#' @param title string of text for title, default
#' Default = "Scatter Plot of Maximum Total Bilirubin versus Maximum Alanine Aminotransferase"
#' @param x_lab string of text for x axis label
#' Default = "Maximum Alanine Aminotransferase (/ULN)"
#' @param y_lab string of text for y axis label
#' Default = "Maximum Total Bilirubin (/ULN)"
#'
#' @details there is no equivalent STREAM output
#'
#' @return ggplot object
#'
#' @importFrom stringr str_wrap
#' @importFrom rlang .data
#' @importFrom gtable gtable_add_grob
#' @importFrom grid grid.draw gpar grid.text
#'
#' @export
#'
#' @template author_withycok
#' @template author_frankla4
#' @template author_holmesw
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' library(tidyr)
#'
#' #Note: CRP is being used in place of Bilirubin here becasue this is the only available data
#' #available in random.cdisc.data
#' anrhi <- data.frame(ANRHI = c(50,50), PARAMCD = c("ALT","CRP"))
#' adsl <- radsl(N=30)
#' adlb <- radlb(adsl) %>% mutate(ANRHI = 50)
#'
#' # Example 1, - Hy's law template (3 and 2 X ULN)
#' g_hy_law(
#'   id = adlb$USUBJID,
#'   term = adlb$PARAMCD,
#'   aval = adlb$AVAL,
#'   arm = adlb$ARM,
#'   term_selected = c("ALT","CRP"),
#'   anrhi = adlb$ANRHI,
#'   folds = c(3,2),
#'   text = c("Normal Range", "Hyperbilirubinemia", "Possible Hy's Law Range", "Temple's Corollary"),
#'   caption = paste("Maximum values are those maximum values that occur",
#'   "post-baseline (no time constraints and not necessarily concurrent events)."),
#'   title = "Scatter Plot of Maximum Total Bilirubin versus Maximum Alanine Aminotransferase",
#'   xlab = "Maximum Alanine Aminotransferase (/ULN)",
#'   ylab = "Maximum Total Bilirubin (/ULN)"
#' )
#'
#' # Example 2, - change the quadrant lines and labels
#' g_hy_law(
#'   id = adlb$USUBJID,
#'   term = adlb$PARAMCD,
#'   aval = adlb$AVAL,
#'   arm = adlb$ARM,
#'   term_selected = c("ALT","CRP"),
#'   anrhi = adlb$ANRHI,
#'   folds = c(10,15),
#'   text = c("Quadrant 1", "Quadrant 2", "Quadrant 3", "Quadrant 4"),
#'   caption = paste("Maximum values are those maximum values that occur",
#'   "post-baseline (no time constraints and not necessarily concurrent events)."),
#'   title = "Scatter Plot of Maximum Total Bilirubin versus Maximum Alanine Aminotransferase",
#'   xlab = "Maximum Alanine Aminotransferase (/ULN)",
#'   ylab = "Maximum Total Bilirubin (/ULN)"
#' )
#'

g_hy_law <- function(id,
                    term,
                    aval,
                    arm,
                    term_selected,
                    anrhi,
                    folds = c(3, 2),
                    text = c("Normal Range", "Hyperbilirubinemia", "Possible Hy's Law Range", "Temple's Corollary"),
                    caption = paste("Maximum values are those maximum values that occur",
                                    "post-baseline (no time constraints and not necessarily concurrent events)."),
                    title = "Scatter Plot of Maximum Total Bilirubin versus Maximum Alanine Aminotransferase",
                    xlab = "Maximum Alanine Aminotransferase (/ULN)",
                    ylab = "Maximum Total Bilirubin (/ULN)"
                    ) {

  anl <- data.frame(id, term, aval, arm, anrhi)

  anl <- anl %>%
    filter(term %in% term_selected) %>%
    group_by(id, term) %>%
    mutate(MAX = max(aval)) %>%
    slice(1) %>%
    mutate(ULN = MAX / anrhi) %>%
    pivot_wider(id_cols = c(id, arm), names_from = term, values_from = ULN)

p <- ggplot(data = anl) +

scale_x_continuous(
    name = xlab,
    breaks = log10(c(seq(0.1, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))),
    limits = c(-1, 2),
    labels = c(0.1, rep(" ", 8), 1, rep(" ", 8), 10, rep(" ", 8), 100),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = ylab,
    breaks = log10(c(seq(0.1, 1, 0.1), seq(2, 10, 1), seq(20, 100, 10))),
    limits = c(-1, 2),
    labels = c(0.1, rep(" ", 8), 1, rep(" ", 8), 10, rep(" ", 8), 100),
    expand = c(0.01, 0.01)
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
    aes(x = log10(0), y = log10(folds[2]), xend = log10(65), yend = log10(folds[2])),
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
  annotate("text", label = paste0(folds[1], "XULN"), x = log10(folds[1]), y = log10(90)) +
  annotate("text", label = paste0(folds[2], "XULN"), x = log10(85), y = log10(folds[2])) +
  annotate("text", label = text[1], x = log10(0.2), y = log10(0.12)) +
  annotate("text", label = text[2], x = log10(0.2), y = log10(80)) +
  annotate("text", label = text[3], x = log10(40), y = log10(80)) +
  annotate("text", label = text[4], x = log10(40), y = log10(0.12)) +
  geom_point(aes(x = log10(.data[[term_selected[1]]]),
                 y = log10(.data[[term_selected[2]]]),
                 shape = arm,
                 color = arm)) +
  scale_shape_manual(values = c(1:length(unique(arm))))

g <- ggplotGrob(p)

grid.newpage()
grid.draw(g)
invisible(g)
}
