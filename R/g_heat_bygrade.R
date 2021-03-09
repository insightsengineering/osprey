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
#' @param conmed_data (`dataframe`)\cr
#' @param conmed_var (`character`)\cr
#' @param xlab (`character`)\cr
#' @param title (`character`)\cr
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
#' # add AVISIT in ADAE
#' ADAE$AVISIT <- sample(unique(ADEX$AVISIT[!is.na(ADEX$AVISIT)]), nrow(ADAE), TRUE)
#' # include three conmed levels
#' ADCM_labs <- rtables::var_labels(ADCM)
#' ADCM <- ADCM %>%
#'   filter(
#'     CMDECOD == "medname A_1/3" | CMDECOD == "medname A_2/3" | CMDECOD == "medname A_3/3"
#'     ) %>%
#'   mutate(CMDECOD = factor(CMDECOD, levels = unique(CMDECOD)))
#' rtables::var_labels(ADCM) <- ADCM_labs
#' exp_data <- ADEX %>% filter(PARCAT1 == "INDIVIDUAL")
#' anno_data <- ADSL
#' anno_var <- c("SEX","COUNTRY")
#' heat_data <- ADAE
#' heat_color_var <- "AETOXGR"
#' heat_color_opt <- c("0" = "gray90","1" = "lightsteelblue1", "2" = "steelblue1", "3" = "steelblue4",
#'                     "4" = "maroon", "5" = "brown4")
#' conmed_data <- ADCM
#' conmed_var <- "CMDECOD"
#'
#' xlab = "Visit"; title = "Heatmap by Grade"
#' g_heat_bygrade(
#'   exp_data,
#'   anno_data,
#'   anno_var,
#'   heat_data,
#'   heat_color_var,
#'   heat_color_opt,
#'   conmed_data,
#'   conmed_var
#'   )
g_heat_bygrade <- function(exp_data,
                           anno_data,
                           anno_var,
                           heat_data,
                           heat_color_var,
                           heat_color_opt,
                           conmed_data,
                           conmed_var,
                           xlab = "Visit",
                           title = "Heatmap by Grade") {
  # check if all PARCAT1 in exp_data is "individual"
  stop_if_not(
    list(is.data.frame(exp_data)),
    list(!is.na(exp_data$AVISIT), "invalid argument: please only include 'INDIVIDUAL' record in exp_data"),
    list(is.data.frame(anno_data)),
    list(all(anno_var %in% names(anno_data)), "invalid argument: anno_var should be variable(s) from anno_data"),
    list(is.data.frame(heat_data)),
    list(
      is_character_single(heat_color_var) && heat_color_var %in% names(heat_data),
      "invalid argument: heat_color_var should be a varaible name in heat_data"
      ),
    list(is_character_vector(heat_color_opt)),
    list(is.data.frame(conmed_data)),
    list(
      length(levels(conmed_data[[conmed_var]])) <= 3,
      "invalid argument: please only include no more than three conmeds for plotting")
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
  # does ongoing data
  exp_lst <- exp_data %>%
    filter(PARAMCD == "DOSE") %>%
    filter(str_to_upper(EOSSTT) == "ONGOING") %>%
    arrange(AVISIT) %>%
    slice_tail() %>%
    select(USUBJID, SUBJ, AVISIT)

  visit_levels <- unique(anl_data$AVISIT)

  conmed_data <- conmed_data %>%
    left_join(anl_data, by = "USUBJID") %>%
    group_by(USUBJID) %>%
    mutate(
      conmed_num = as.numeric(CMDECOD),
      conmed_num_m = median(unique(conmed_num), na.rm = TRUE),
      distance = (ifelse(conmed_num <= conmed_num_m, conmed_num - 1, conmed_num + 1) - conmed_num_m) / 5,
      conmed_x = as.numeric(factor(AVISIT, levels = visit_levels)) + distance,
      SUBJ = tail(strsplit(USUBJID, "-")[[1]], n = 1)
      ) %>%
    select(USUBJID, SUBJ, conmed_x, !!conmed_var)


  subj_levels <- unique(anl_data$SUBJ)
  p <- ggplot(
    data = anl_data,
    aes(x = AVISIT, y = factor(SUBJ, levels = c(subj_levels, "")))
    ) +
    geom_tile(aes(fill = heat_color_max)) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_manual(
      name = "Highest grade of\nindividual events",
      values = heat_color_opt
      ) +
    # plot dose reduction
    geom_segment(
      data = ex_red,
      aes(
        y = as.numeric(factor(SUBJ, levels = rev(subj_levels))) + 0.3,
        x = AVISIT,
        yend = as.numeric(factor(SUBJ, levels = rev(subj_levels))) - 0.3,
        xend = AVISIT
        ),
      arrow = arrow(length = unit(0.1, "cm")),
      size = .5,
      color = "black"
      ) +
    # plot ongoing
    geom_segment(
      data = exp_lst,
      aes(
        x = as.numeric(factor(AVISIT, levels = visit_levels)) + 0.5,
        xend = as.numeric(factor(AVISIT, levels = visit_levels)) + 0.65,
        y = as.numeric(factor(SUBJ, levels = rev(subj_levels))),
        yend = as.numeric(factor(SUBJ, levels = rev(subj_levels)))
        ),
      arrow = arrow(length = unit(0.1, "cm")),
      size = .5,
      color = "black"
      ) +
    # plot conmed
    geom_point(
      data = conmed_data,
      aes(
        x = conmed_x,
        y = as.numeric(factor(SUBJ, levels = rev(subj_levels))),
        shape = conmed_data[[conmed_var]]
        ),
      size = 3
      ) +
    scale_shape(name = rtables::var_labels(conmed_data)[conmed_var]) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 90),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    ) +
    labs(x = xlab, y = "ylab")
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
