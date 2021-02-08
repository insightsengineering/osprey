heat_color <- ADAE$AETOXGR
heat_id <- ADAE$USUBJID
heat_visit <- ADAE$AVISIT

exp_data <- ADEX %>% filter(PARCAT1 == "INDIVIDUAL")
anno_data <- ADSL
anno_var <- c("SEX","RACE","COUNTRY")
heat_data <- ADAE
heat_color_var <- "AETOXGR"
heat_color_name <- "Highest grade of\nindividual events"
heat_color_opt <- c("0" = "aliceblue","1" = "lightsteelblue1", "2" = "steelblue1", "3" = "steelblue4",
                    "4" = "maroon", "5" = "black")
g_ce_heat_bygrade <- function(exp_data,
                              anno_data,
                              heat_data,
                              heat_color_var,
                              heat_color_opt,
                              heat_visit,
                              xlab = "Visit",
                              ylab = "Patient"){
  # check if all PARCAT1 in exp_data is "individual"
  stop_if_not(
    list(!is.na(exp_data$AVISIT), "Please only include 'INDIVIDUAL' record in exp_data")
  )

  heat_data <- heat_data %>%
    select(USUBJID, AVISIT, (!!heat_color_var))

  anno_data <- anno_data %>%
    select(!!anno_var, USUBJID) %>%
    mutate(SUBJ = substr(USUBJID, 16, 20))

  anl_data <- exp_data %>%
    select(USUBJID, AVISIT) %>%
    left_join(heat_data, by = c("USUBJID", "AVISIT")) %>%
    distinct() %>%
    mutate(heat_color_num = tidyr::replace_na(as.numeric(.data[[heat_color_var]]),0)) %>%
    group_by(USUBJID, AVISIT) %>%
    mutate(heat_color_max = factor(max(heat_color_num), c("0",levels(.data[[heat_color_var]])))) %>%
    select(-(!!heat_color_var), -heat_color_num) %>%
    distinct() %>%
    left_join(anno_data, by = "USUBJID")

  p <- ggplot(data = exp_data, aes(x = AVISIT,
                                   y = USUBJID)) +
    geom_tile(aes(fill = heat_color_max)) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_manual(name = heat_color_name,
                      values = heat_color_opt) +
    # # plot dose reduction
    # geom_segment(data = ex_red,
    #              aes(y = as.numeric(factor(SUBJ, levels = subj_levels)) + 0.3,
    #                  xend = AVISITCD, yend = as.numeric(factor(SUBJ, levels = subj_levels)) - 0.3),
    #              arrow = arrow(length = unit(0.1, "cm")), size = .5, color = "black") +
    # # plot ongoing
    # geom_segment(data = exp_lst,
    #              aes(x = as.numeric(factor(AVISITCD, levels = visit_levels)) + 0.5,
    #                  xend = as.numeric(factor(AVISITCD, levels = visit_levels)) + 0.65,
    #                  y = as.numeric(factor(SUBJ, levels = subj_levels)),
    #                  yend = as.numeric(factor(SUBJ, levels = subj_levels)) ),
    #              arrow = arrow(length = unit(0.1, "cm")), size = .5, color = "black") +
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
      y = ylab
     )
  #plot left legend
  t <- anl_data

}
