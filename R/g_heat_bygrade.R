#' Heatmap by Grade
#'
#' This function plots heatmap
#'
#' @param exp_data (`dataframe`)\cr contains subject identifier. Usually it is \code{ADSL$USUBJID}.
#' @param anno_data (`dataframe`)\cr to specify color for the heatmap plot.
#' For example \code{ADAE$AETOXGR} or \code{ADCE$CETOXGR}.
#' @param anno_var (`vector`)\cr
#' @param heat_data (`dataframe`)\cr contains the information needed for the text over heatmap
#' Usually is \code{ADCM}.
#' @param heat_color_var (`character`)\cr
#' @param heat_color_opt (`vector`)\cr
#' @import ggplot2
#' @importFrom stringr str_to_upper
#' @importFrom tidyr replace_na
#'
#' @export
#'
#' @author Nina Qi (qit3) \email{qit3@gene.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(N = 30)
#' ADEX <- radex(ADSL)
#' ADAE <- radae(ADSL)
#' ADCM <- radcm(ADSL)
#' #add AVISIT in ADAE
#' ADAE$AVISIT <- sample(unique(ADEX$AVISIT[!is.na(ADEX$AVISIT)]), nrow(ADAE), TRUE)
#' exp_data <- ADEX %>% filter(PARCAT1 == "INDIVIDUAL")
#' anno_data <- ADSL
#' anno_var <- c("SEX","COUNTRY")
#' heat_data <- ADAE
#' heat_color_var <- "AETOXGR"
#' heat_color_name <- "Highest grade of\nindividual events"
#' heat_color_opt <- c("0" = "gray90","1" = "lightsteelblue1", "2" = "steelblue1", "3" = "steelblue4",
#'                     "4" = "maroon", "5" = "brown4")
#' xlab = "Visit"; title = "Heatmap by Grade"
#' g_heat_bygrade(
#'   exp_data,
#'   anno_data,
#'   anno_var,
#'   heat_data,
#'   heat_color_var,
#'   heat_color_opt
#'   )
g_heat_bygrade <- function(exp_data,
                           anno_data,
                           anno_var,
                           heat_data,
                           heat_color_var,
                           heat_color_opt,
                           xlab = "Visit",
                           title = "Heatmap by Grade"){
  # check if all PARCAT1 in exp_data is "individual"
  stop_if_not(
    list(!is.na(exp_data$AVISIT), "Please only include 'INDIVIDUAL' record in exp_data")
  )

  heat_data <- heat_data %>%
    select(USUBJID, AVISIT, (!!heat_color_var))

  anno_data <- anno_data %>%
    select(!!anno_var, USUBJID) %>%
    group_by(USUBJID) %>%
    mutate(SUBJ = tail(strsplit(USUBJID, "-")[[1]], n = 1))

  anl_data <- exp_data %>%
    select(USUBJID, AVISIT) %>%
    left_join(heat_data, by = c("USUBJID", "AVISIT")) %>%
    distinct() %>%
    mutate(heat_color_num = tidyr::replace_na(as.numeric(.data[[heat_color_var]]), 0)) %>%
    group_by(USUBJID, AVISIT) %>%
    arrange(AVISIT) %>%
    mutate(heat_color_max = factor(max(heat_color_num), c("0", levels(.data[[heat_color_var]])))) %>%
    select(- (!!heat_color_var), -heat_color_num) %>%
    distinct() %>%
    left_join(anno_data, by = "USUBJID")

  exp_data <- exp_data %>%
    group_by(USUBJID) %>%
    mutate(SUBJ = tail(strsplit(USUBJID, "-")[[1]], n = 1))

  #dose reduction data
  ex_red <- exp_data %>%
    #??is this filter needed??
    filter(PARAMCD == "DOSE") %>%
    group_by(USUBJID) %>%
    arrange(ASTDTM) %>%
    mutate(
      RANK = order(ASTDTM),
      LASTDOSE = lag(AVAL),
      DOSERED = ifelse(RANK != 1 & AVAL < LASTDOSE, "Y", "")
      ) %>%
    select(USUBJID, SUBJ, AVISIT, RANK, AVAL, LASTDOSE, DOSERED) %>%
    filter(DOSERED == "Y")
  #does ongoing data
  exp_lst <- exp_data %>%
    filter(PARAMCD == "DOSE") %>%
    filter(str_to_upper(EOSSTT) == "ONGOING") %>%
    arrange(AVISIT) %>%
    slice_tail() %>%
    select(USUBJID, SUBJ, AVISIT)


  subj_levels <- unique(anl_data$SUBJ)
  visit_levels <- unique(anl_data$AVISIT)
  p <- ggplot(data = anl_data, aes(x = AVISIT,
                                   y = factor(SUBJ, levels = c(subj_levels, "")))) +
    geom_tile(aes(fill = heat_color_max)) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_manual(name = heat_color_name,
                      values = heat_color_opt) +
    # plot dose reduction
    geom_segment(
      data = ex_red,
      aes(
        y = as.numeric(factor(SUBJ, levels = rev(subj_levels))) + 0.3,
        x = AVISIT,
        yend = as.numeric(factor(SUBJ, levels = rev(subj_levels))) - 0.3,
        xend = AVISIT
        ),
      arrow = arrow(length = unit(0.1, "cm")), size = .5, color = "black"
      ) +
    # plot ongoing
    geom_segment(data = exp_lst,
                 aes(x = as.numeric(factor(AVISIT, levels = visit_levels)) + 0.5,
                     xend = as.numeric(factor(AVISIT, levels = visit_levels)) + 0.65,
                     y = as.numeric(factor(SUBJ, levels = rev(subj_levels))),
                     yend = as.numeric(factor(SUBJ, levels = rev(subj_levels)))),
                 arrow = arrow(length = unit(0.1, "cm")), size = .5, color = "black") +
    # # if there's atezo IRR, add in text
    # geom_text(aes(x = as.numeric(factor(AVISITCD, levels = visit_levels)) + 0.2,
    #               label = ATEZOEXP),
    #           color = "white", size = 2, fontface = "bold", na.rm = T) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 90),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    ) +
    labs(
      x = xlab,
      y = "ylab"
     )
  # plot title and labels
  if (!is.null(title)) {
    p <- p +
      labs(title = title) +
      theme(plot.title = element_text(face = "bold"))
  }

  #plot left legend
  t <- as.data.frame(anl_data[, c(anno_var, "SUBJ")]) %>% distinct()
  my_theme <- ttheme_default(
    core = list(
      bg_params = list(fill = NA, col = NA),
      fg_params = list(cex = 0.8)
    ),
    colhead = list(
      bg_params = list(fill = NA, col = NA),
      fg_params = list(cex = 0.8)
    )
  )
  tb <- tableGrob(t, rows = NULL, theme = my_theme)
  tb$heights <- unit(rep(1 / nrow(tb), nrow(tb)), "null")

  # grab plot and table as one plot
  g0 <- ggplotGrob(p)
  g1 <- gtable_add_cols(g0, sum(tb$widths), 0)
  g <- gtable_add_grob(g1, tb, t = g1$layout[g1$layout$name == "panel", 1], l = 1)

  grid.newpage()
  grid.draw(g)
  invisible(g)
}
