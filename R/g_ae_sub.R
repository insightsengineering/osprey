#' Adverse Event Category Plot
#'
#' Draw adverse event category plot.
#'
#' @param term event term vector.
#' @param term_selected The selected term that is passed to analysis. Default set to the first value in term vector.
#' @param id id of the event term vector. Ususally it is USUBJID.
#' @param arm Treatment variable. Usually it is ACTARM
#' @param arm_sl Subject level treatment variable. Usually it is ACTARM
#' @param col_by_levels Selected levels treatment and baseline respectively.
#' @param subgroups Variables to conduct analysis.
#' @param subgroups_sl Subject level variables to conduct analysis.
#' @param subgroups_levels Named list of variables to conduct analysis.
#' The names of the nested lists are used to show as the label.
#' @param conf_level The confidence interval level, default set to 0.95.
#' @param cimethod The method used to calculate confidence interval. Defalt is "wald".
#' Possible choices are methods supported in `DescTools::BinomDiffCI`.
#' @param fontsize The size of the font. It is not the true font size but a multiplier.
#' @param title  The title to be shown in the graph.
#' @param foot The footnote to be shown in the graph.
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#'
#' @details there is no equivalent STREAM output
#'
#' @return grob object
#'
#' @import dplyr
#' @import ggplot2
#' @import checkmate
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid unit.c unit
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_dup str_replace_all str_c str_detect
#' @importFrom DescTools BinomDiffCI
#' @export
#'
#' @examples
#' library(osprey)
#' library(random.cdisc.data)
#'
#' term <- as.character(cadae$AEDECOD)
#' id <- cadae$USUBJID
#' arm <- as.character(cadae$ACTARMCD)
#' arm_sl <- as.character(cadsl$ACTARMCD)
#' subgroups_sl <- cadsl[, c("SEX", "RACE", "STRATA1")]
#' subgroups <- cadae[, c("SEX", "RACE", "STRATA1")]
#' trt <- "ARM A"
#' ref <- "ARM C"
#' subgroups_labels <- list(RACE = list("Total" = "Race", "AMERICAN INDIAN OR ALASKA NATIVE" = "American", "WHITE" = "White", "ASIAN" = "Asian",
#' "African" = "BLACK OR AFRICAN AMERICAN"), STRATA1 = list("Total" = "Strata", "A" = "TypeA", "B" = "TypeB", "C" = "Typec"),
#' SEX = list("Total" = "Sex", "M" = "Male", "F" = "Female", "U" = "Unknown"))
#' term_selected <- unique(term)[1]
#' g_ae_sub(term, term_selected, id, arm, arm_sl, subgroups, subgroups_sl, subgroups_labels = subgroups_labels)

g_ae_sub <- function(term,
                     term_selected,
                     id,
                     arm,
                     arm_sl,
                     subgroups,
                     subgroups_sl,
                     trt = unique(arm)[1],
                     ref = unique(arm)[2],
                     indent = 4,
                     subgroups_labels = NULL,
                     xmax = 0,
                     cimethod = "wald",
                     conf_level = 0.95,
                     fontsize = 4,
                     draw = FALSE) {
  if (is.null(subgroups)) {
    return(textGrob("No Subgroups Selected"))
  }
  if (is.null(subgroups_labels)) {
    subgroups_names <- paste(colnames(subgroups), collapse = ", ")
  } else {
    subgroups_names <-
      paste(sapply(subgroups_labels, function(x)
        x$Total), collapse = ", ")

    labels <- unlist(subgroups_labels)
    label_df <-
      tibble(level = str_replace_all(names(labels), "\\.", "__"),
             label = labels) %>%
      bind_rows(c(level = "TOTAL__Total", label = "Overall")) %>%
      mutate(indents = str_dup(" ", if_else(
        str_detect(level, "__Total"), 0, indent
      )),
      label = paste0(indents, label))
  }

  if (is.null(term_selected)) {
    term_selected <- unique(term)
  }

  l <- length(id)
  assert_vector(id, min.len = 1)
  assert_character(term, len = l)
  assert_character(arm, len = l)
  assert_vector(arm_sl, min.len = 2)
  arm_unique <- unique(arm)
  assert_choice(trt, arm_unique)
  assert_choice(ref, arm_unique)
  assert_subset(term_selected, unique(term))
  assert_choice(cimethod,
                c(
                  'wald',
                  'waldcc',
                  'ac',
                  'scorecc',
                  'score',
                  'mn',
                  'mee',
                  'blj',
                  'ha'
                ))
  assert_data_frame(subgroups, nrows = l)
  assert_data_frame(subgroups_sl, nrows = length(arm_sl))
  assert_number(conf_level, lower = 0.5, upper = 1)
  assert_number(fontsize, lower = 0)
  assert_number(indent, lower = 0)
  assert_number(xmax, lower = 0)

  subgroups <- subgroups %>%
    mutate_all(as.character) %>%
    mutate(TOTAL = "Total")

  arms <- c(trt, ref)
  xs <- syms(paste("count", arms, sep = "__"))
  ns <- syms(paste("total", arms, sep = "__"))
  rs <- syms(paste("risk", arms, sep = "__"))

  x1 <- xs[[1]]
  n1 <- ns[[1]]
  x2 <- xs[[2]]
  n2 <- ns[[2]]
  r1 <- rs[[1]]
  r2 <- rs[[2]]

  df_ref <- tibble(arm = c(ref, trt))

  df <- cbind(id = id,
              term = term,
              arm = arm,
              subgroups) %>%
    filter(term %in% term_selected & arm %in% c(ref, trt)) %>%
    select(-term) %>%
    unique %>%
    pivot_longer(
      names_to = "strata",
      cols = colnames(subgroups),
      values_to = "value"
    ) %>%
    group_by(arm, strata, value) %>%
    summarise(n = n()) %>%
    full_join(df_ref, by = "arm") %>%
    replace_na(list(n = 0)) %>%
    ungroup %>%
    pivot_wider(
      id_cols = c("strata", "value"),
      names_from = "arm",
      values_from = "n",
      names_prefix = "count__",
      values_fill = list(n = 0)
    )

  df_sl <- cbind(arm = arm_sl, subgroups_sl) %>%
    filter(arm %in% c(ref, trt)) %>%
    mutate(TOTAL = "Total") %>%
    pivot_longer(
      names_to = "strata",
      cols = c(colnames(subgroups_sl), "TOTAL"),
      values_to = "value"
    ) %>%
    group_by(arm, strata, value) %>%
    summarise(n = n()) %>%
    ungroup %>%
    pivot_wider(
      id_cols = c("strata", "value"),
      names_from = "arm",
      values_from = "n",
      names_prefix = "total__",
      values_fill = list(n = 0)
    )

  df <- df %>%
    left_join(df_sl, by = c("strata", "value")) %>%
    group_by(strata, value) %>%
    mutate(
      !!r1 := !!x1 / !!n1,
      !!r2 := !!x2 / !!n2,
      lower = BinomDiffCI(!!x1, !!n1, !!x2, !!n2, conf_level, method = cimethod)[2],
      upper = BinomDiffCI(!!x1, !!n1, !!x2, !!n2, conf_level, method = cimethod)[3],
      riskdiff = !!r1-!!r2
    ) %>%
    pivot_longer(matches("__"),
                 names_to = c(".value", "arm"),
                 names_sep = "__") %>%
    ungroup %>%
    unite("level", strata, value, remove = FALSE, sep = "__")

  level_format_df <- df %>%
    select(strata, value) %>%
    unique %>%
    group_by(strata) %>%
    summarise(value = paste(c("Total", value), collapse = ",")) %>%
    separate_rows(value, sep = ",") %>%
    unique %>%
    unite("level", strata, value, sep = "__", remove = FALSE) %>%
    mutate(order = if_else(strata == "TOTAL", integer(1), -row_number())) %>%
    arrange(order)

  if (is.null(subgroups_labels)) {
    level_format_df <- level_format_df %>%
      mutate(label = if_else(
        strata == "TOTAL",
        "Overall",
        if_else(value == "Total", strata, paste0(str_c(
          rep(" ", indent), collapse = ""
        ), value))
      ))
  } else {
    level_format_df <- level_format_df %>%
      inner_join(label_df, by = "level")
  }

  df <- df %>%
    semi_join(level_format_df, by = "level")
  mytheme <-
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major.y = element_blank(),
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.ticks = element_blank(),
      axis.text.y = element_text(
        face = "bold",
        size = fontsize * .pt,
        hjust = 0,
        color = "black"
      ),
      axis.text.x = element_text(
        face = "bold",
        size = fontsize * .pt,
        color = "black"
      ),
      text = element_text(size = fontsize * .pt),
      legend.text = element_text(size = fontsize * 2.5),
      plot.title = element_text(hjust = 0.5)
    )


  y_axis <-
    scale_y_discrete(
      limits = level_format_df$level,
      breaks = level_format_df$level,
      labels = level_format_df$label
    )

  df_risk <- df %>%
    select(level, lower, upper, riskdiff) %>%
    unique
  if (xmax <= 0) {
    xmax <- max(abs(df_risk$upper), abs(df_risk$lower), na.rm = T)
  }
  p1 <-
    ggplot(df_risk) + geom_point(aes(x = riskdiff, y = level), size = fontsize) +
    geom_vline(
      data = NULL,
      xintercept = 0,
      linetype = 1,
      color = "grey"
    ) +
    geom_errorbarh(aes(xmin = lower, xmax = upper, y = level), height = 0.3) +
    mytheme +
    theme(axis.ticks.x = element_line(),
          axis.line.x = element_line()) +
    y_axis +
    coord_cartesian(xlim = c(-xmax, xmax))
  p1_grob <- ggplotGrob(p1)

  df_total <- df %>%
    group_by(level) %>%
    summarise(n = sum(total)) %>%
    mutate(percent = n / length(arm_sl) * 100)

  p2 <- ggplot(df_total) +
    geom_text(aes(
      x = "Patients(%)",
      y = level,
      label = sprintf("%i (%.1f)", n, percent)
    ), size = fontsize) +
    mytheme +
    y_axis +
    scale_x_discrete(position = "top")

  p2_grob <- ggplotGrob(p2)

  df_ci <- df %>%
    pivot_wider(
      names_from = "arm",
      values_from = "risk",
      id_cols = c("level", "lower", "upper", "riskdiff")
    ) %>%
    pivot_longer(
      names_to = "x",
      values_to = "v",
      cols = c("lower", "upper", "riskdiff", trt, ref)
    ) %>%
    mutate(vlabel = sprintf("%.2f", v))


  labels <- c("TRT", "CONT", "Diff", "Lower", "Upper")
  p3 <- ggplot(df_ci) +
    geom_text(aes(x = x, y = level, label = vlabel), size = fontsize) +
    mytheme +
    y_axis +
    scale_x_discrete(
      limits = c(trt, ref, "riskdiff", "lower", "upper"),
      labels = labels,
      position = "top"
    )

  p3_grob <- ggplotGrob(p3)

  grobs <- grob_parts(p1_grob, "axis-l")
  grobs <- append(grobs, grob_parts(p2_grob, c("axis-t", "panel")))
  grobs <- append(grobs, grob_parts(p1_grob, c("panel", "axis-b")))
  grobs <- append(grobs, grob_parts(p3_grob, c("axis-t", "panel")))
  less_risk <- textGrob(
    "Favor\nTreatment",
    just = "centre",
    x = unit(fontsize * .pt, "pt"),
    gp = gpar(fontsize = fontsize * .pt, fontface = "bold")
  )
  more_risk <- textGrob(
    "Favor\nControl",
    just = "centre",
    x = unit(1, "npc") - unit(fontsize * .pt, "pt"),
    gp = gpar(fontsize = fontsize * .pt, fontface = "bold")
  )
  risk_label <- arrangeGrob(less_risk, more_risk, nrow = 1)
  grobs <- append(grobs, list(
    textGrob(
      "Risk Difference (CI)",
      just = "centre",
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      gp = gpar(fontsize = fontsize * .pt, fontface = "bold")
    ),
    risk_label
  ))

  widths = unit.c(grobWidth(grobs[[1]]),
                  unit(c(14 * fontsize, 1, 50 * fontsize),
                       c("pt", "null", "pt")))
  heights = unit.c(
    grobHeight(grobs[[6]]),
    unit(1, "null"),
    grobHeight(grobs[[5]]),
    unit(fontsize * .pt * 2, "pt")
  )


  boldfont <- gpar(fontsize = fontsize * 4,
                   fontface = "bold",
                   lineheight = 1)
  layout_matrix <- rbind(c(NA, 2, 8, 6),
                         c(1, 3, 4, 7),
                         c(NA, NA, 5, NA),
                         c(NA, NA, 9, NA))
  ret <- arrangeGrob(
    grobs = grobs,
    layout_matrix = layout_matrix,
    heights = heights,
    widths = widths,
    clip = "on"
  )
  if (draw) {
    plot(ret)
  }
  invisible(ret)
}
