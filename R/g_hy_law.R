#' Hy's Law Plot
#'
#' A scatter plot typically used to display maximum total bilirubin values (TBL)
#' versus maximum alanine transaminase (ALT) values.
#'
#' @param id unique subject identifier.
#' @param term the term of the observation.
#' @param aval analysis value.
#' @param arm treatment arm. Used as fill color in the plot.
#' @param term_selected string vector of length 2 - the two terms selected to be
#'   used in the plot. First value corresponds to parameter plotted on the
#'   x-axis. Second value corresponds to that plotted on the y-axis.
#' @param anrhi the high limit of normal range.
#' @param folds numeric vector of length two. Indicates the position of the
#'   reference lines to be drawn. Default c(3, 2) corresponds to a line at
#'   position 3 on the x-axis and 2 on the y-axis.
#' @param text string vector of length four with the label to be shown on each
#'   quadrant. First value corresponds to label shown in the bottom left
#'   quadrant. Subsequent values move through the graph clockwise.
#' @param caption string of text for footnote. Details of methodology can be
#'   shown here.
#' @param title string of text for plot title.
#' @param xlab string of text for x axis label.
#' @param ylab string of text for y axis label.
#'
#' @details
#' This graphic is based upon the eDISH (evaluation of Drug Induced Serious
#' Hepatotoxicity) plot of Watkins et. al. in a 2008 publication from Hepatology.
#' Maximum values are defined as the maximum post-baseline value at any time
#' during the entire length of the observation period. Both axes are in log
#' scale to control for the dispersion of the data. The values are plotted in
#' 'times upper limit of normal' where a value of 1 would mean that the result
#' was normal. Any value above or below 1 would be considered above the upper
#' limit or normal or below the upper limit of normal respectively. For
#' instance, a value of 3 would be read as '3 times the upper limit of normal'.
#' Reference lines are included to determine various states, based upon clinical
#' interpretation of the values and includes the following:
#'
#' * Hyperbilirubinemia TBL at least 2 xULN and ALT less than 3 xULN
#' * Normal Range TBL <= 1 xULN and ALT <= 1xULN
#' * Temple’s Corollary TBL <= 1 xULN and ALT at least 3 xULN
#' * Possible Hy's Law TBL at least 2 xULN and ALT at least 3 xULN
#'
#' This plot can easily be adjusted for other lab parameters and reference
#' ranges as needed. Consultation with a clinical expert to determine which
#' associations would be clinically meaningful and how to interpret those
#' associations is recommended.
#'
#' There is no equivalent STREAM output.
#'
#' @return plot object
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
#' library(scda)
#' library(dplyr)
#'
#' # Note: CRP is being used in place of Bilirubin here because this is the only available data
#' # available in SCDA
#' latest_dfs <- synthetic_cdisc_data("latest")
#' adsl <- latest_dfs[["adsl"]]
#' adlb <- latest_dfs[["adlb"]] %>% mutate(ANRHI = 50)
#'
#' # Example 1, - Hy's law template (3 and 2 X ULN)
#' g_hy_law(
#'   id = adlb$USUBJID,
#'   term = adlb$PARAMCD,
#'   aval = adlb$AVAL,
#'   arm = adlb$ARM,
#'   term_selected = c("ALT", "CRP"),
#'   anrhi = adlb$ANRHI,
#'   folds = c(3, 2),
#'   text = c("Normal Range", "Hyperbilirubinemia", "Possible Hy's Law Range", "Temple's Corollary"),
#'   caption = paste("Maximum values are those maximum values that occur",
#'   "post-baseline (no time constraints and not necessarily concurrent events)."),
#'   title = "Max. Total Bilirubin vs. Max. Alanine Aminotransferase",
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
#'   term_selected = c("ALT", "CRP"),
#'   anrhi = adlb$ANRHI,
#'   folds = c(10, 15),
#'   text = c("Quadrant 1", "Quadrant 2", "Quadrant 3", "Quadrant 4"),
#'   caption = paste("Maximum values are those maximum values that occur",
#'   "post-baseline (no time constraints and not necessarily concurrent events)."),
#'   title = "Max. Total Bilirubin vs. Max. Alanine Aminotransferase",
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
                     caption = paste("Maximum values are those maximum values that occur post-baseline",
                                     "(no time constraints and not necessarily concurrent events)."
                     ),
                     title = "Max. Total Bilirubin vs. Max. Alanine Aminotransferase",
                     xlab = "Maximum Alanine Aminotransferase (/ULN)",
                     ylab = "Maximum Total Bilirubin (/ULN)"
) {

  assert_that(
    is.character(term_selected) && length(term_selected) == 2,
    msg = "invalid argument: term_selected must be a character array of length 2"
  )
  assert_that(
    is.numeric(folds) && length(folds) == 2,
    msg = "invalid argument: folds must be a numeric array of length 2"
  )
  assert_that(
    is.character(text) && length(text) == 4,
    msg = "invalid argument: text must be a character array of length 4"
  )

  character_vars <- c("title", "caption", "xlab", "ylab")

  for (parameter in character_vars) {
    assert_that(is.character(parameter) && length(parameter) == 1,
                msg = paste("invalid argument:", parameter, "must be a string"))
  }

  anl <- data.frame(id, term, aval, arm, anrhi)

  anl <- anl %>%
    dplyr::filter(term %in% term_selected) %>%
    dplyr::group_by(id, term) %>%
    dplyr::mutate(MAX = max(aval)) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(ULN = .data$MAX / anrhi) %>%
    tidyr::pivot_wider(id_cols = c(id, arm), names_from = term, values_from = .data$ULN)

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
    labs(title = title, caption = caption) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot",
      plot.caption = element_text(hjust = 0),
      plot.caption.position = "plot",
      legend.title = element_blank(),
      panel.grid = element_blank()
    ) +
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
                   color = arm)
    ) +
    scale_shape_manual(values = c(1:n_distinct(arm)))

  g <- ggplotGrob(p)
  grid.newpage()
  grid.draw(g)
  invisible(g)
}
