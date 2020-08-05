#' Adverse Event Category Plot
#'
#' Draw adverse event category plot.
#'
#' @param term \code{character} event term vector.
#'  All terms are included by default.
#' @param id \code{vector} id of the event term vector.
#' For example, \code{ADAE$USUBJID}.
#' @param arm A \code{vector} of the treatment variable.
#' For example, \code{ADAE$ACTARM}.
#' @param arm_sl A \code{vector} of the subject level treatment variable.
#' For example, \code{ADSL$ACTARM}.
#' @param subgroups \code{data.frame} Variables to conduct analysis.
#' @param subgroups_sl \code{data.frame} Subject level variables to conduct analysis.
#' Usually from ADSL.
#' @param ref \code{character} indicates the name of the reference arm. Default is the first
#' level of \code{arm}.
#' @param trt \code{character} indicates the name of the treatment arm. Default is the second
#' level of \code{arm}.
#' @param indent \code{numeric} non-negative integer where 0 means that the subgroup levels should not be indented
#' @param subgroups_levels A nested named \code{list} of variables to conduct analysis.
#' The names of the nested lists are used to show as the label.
#' The children lists should start with "Total" = variable label,
#' followed by labels for each level of said variable. See example for reference.
#' @param xmax \code{numeric} maximum range for the x-axis.
#' x-axis range will be automatically assigned based on risk output when xmax is less than or equal to 0.
#' xmax is 0 by default
#' @param conf_level \code{numeric} The confidence interval level, default set to 0.95.
#' @param diff_ci_method \code{character} The method used to calculate confidence interval.
#' Defalt is "wald". Possible choices are methods supported in \code{\link[DescTools]{BinomDiffCI}}.
#' @param fontsize \code{numeric} font size for the plot. It is the size used in ggplot2 with
#' default unit "mm", if you want "points" you will need to devide the point number by
#' \code{ggplot2:::.pt}.
#' @param draw \code{logical} whether to draw the plot.
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#'
#' @details there is no equivalent STREAM output
#'
#' @return grob object
#'
#' @import dplyr
#' @import ggplot2
#' @import utils.nest
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid unit.c unit
#' @importFrom tidyr pivot_longer pivot_wider replace_na unite separate_rows
#' @importFrom stringr str_dup str_replace_all str_c str_detect
#' @importFrom DescTools BinomDiffCI
#' @export
#'
#' @examples
#' library(osprey)
#' library(random.cdisc.data)
#'
#' ADAE <- radae(cached = TRUE)
#' ADSL <- radsl(cached = TRUE)
#'
#' term <- as.character(ADAE$AEDECOD)
#' id <- ADAE$USUBJID
#' arm <- ADAE$ACTARMCD
#' arm_sl <- as.character(ADSL$ACTARMCD)
#' subgroups_sl <- ADSL[, c("SEX", "RACE", "STRATA1")]
#' subgroups <- ADAE[, c("SEX", "RACE", "STRATA1")]
#' trt <- "ARM A"
#' ref <- "ARM C"
#' subgroups_levels <- list(RACE = list("Total" = "Race",
#'                                      "AMERICAN INDIAN OR ALASKA NATIVE" = "American",
#'                                      "WHITE" = "White",
#'                                      "ASIAN" = "Asian",
#'                                      "African" = "BLACK OR AFRICAN AMERICAN"),
#'                          STRATA1 = list("Total" = "Strata",
#'                                         "A" = "TypeA",
#'                                         "B" = "TypeB",
#'                                         "C" = "Typec"),
#'                          SEX = list("Total" = "Sex",
#'                                     "M" = "Male",
#'                                     "F" = "Female",
#'                                     "U" = "Unknown"))
#' g_ae_sub(term,
#'          id,
#'          arm,
#'          arm_sl,
#'          subgroups,
#'          subgroups_sl,
#'          subgroups_levels = subgroups_levels)

g_ae_sub <- function(term,
                     id,
                     arm,
                     arm_sl,
                     subgroups,
                     subgroups_sl,
                     trt = levels(arm)[1],
                     ref = levels(arm)[2],
                     indent = 4,
                     subgroups_levels = NULL,
                     xmax = 0,
                     conf_level = 0.95,
                     diff_ci_method = c(
                       "wald", "waldcc", "ac", "score", "scorecc",
                       "mn", "mee", "blj", "ha", "beal"),
                     fontsize = 4,
                     draw = TRUE) {

  diff_ci_method <- match.arg(diff_ci_method)
  stop_if_not(
    list(!is_empty(term), "missing argument: term must be specified"),
    list(!is_empty(id), "missing argument: id must be specified"),
    list(!is_empty(arm), "missing argument: arm must be specified"),
    list(!is_empty(arm_sl), "missing argument: arm_sl must be specified"),
    list(!is_empty(subgroups), "missing argument: subgroups must be specified")
  )
  if (!is.null(subgroups_levels)) {
    labels <- unlist(subgroups_levels)
    label_df <-
      tibble(level = str_replace_all(names(labels), "\\.", "__"),
             label = labels) %>%
      bind_rows(c(level = "TOTAL__Total", label = "Overall")) %>%
      mutate(indents = str_dup(" ", if_else(
        str_detect(level, "__Total"), 0, indent
      )),
      #create label with indents if not total
      label = paste0(indents, label))
  }
  stop_if_not(
    list(length(unique(vapply(list(id, term, arm),
                              length, integer(1)))) == 1,
         "invalid arguments: check that the length of id, term and arm are identical"
    ),
    list(is_character_vector(arm_sl, min_length = 2),
         "invalid argument: check that arm_sl is a character vector with length >= 2"
    ),
    list(all(c(trt, ref) %in% unique(arm)),
         "invalid arguments: trt and ref need to be from arm"
    ),
    list(
      is_numeric_single(conf_level) & between(conf_level, 0.5, 1),
      "invalid argument: conf_level should be a number between 0.5 and 1"
    ),
    list(
      is_numeric_single(fontsize) & between(fontsize, 0, Inf),
      "invalid argument: check that fontsize is a number greater than 0"
    ),
    list(
      "data.frame" %in% class(subgroups) & nrow(subgroups) == length(arm),
      "invalid argument: subgroups needs to be a data.frame with nrow = length(arm)"
    ),
    list(
      "data.frame" %in% class(subgroups_sl) & nrow(subgroups_sl) == length(arm_sl),
      "invalid argument: subgroups_sl need to be a data.frame with nrow = length(arm_sl)"
    ),
    list(is_numeric_single(indent) & between(indent, 0, Inf),
         "invalid argument: indent must be a number >= 0"),
    list(is_numeric_single(xmax),
         "invalid argument: xmax must be a number")
  )


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

  df <- cbind(id = id, term = term, arm = arm, subgroups) %>%
    filter(arm %in% c(ref, trt)) %>%
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
      lower = BinomDiffCI(!!x1, !!n1, !!x2, !!n2,
                          conf_level, method = diff_ci_method)[2],
      upper = BinomDiffCI(!!x1, !!n1, !!x2, !!n2,
                          conf_level, method = diff_ci_method)[3],
      riskdiff = !!r1 - !!r2
    ) %>%
    pivot_longer(
      matches("__"),
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

  if (is.null(subgroups_levels)) {
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
    ggplot(df_risk) +
    geom_point(aes(x = riskdiff,
                   y = level),
               size = fontsize) +
    geom_vline(
      data = NULL,
      xintercept = 0,
      linetype = 1,
      color = "grey"
    ) +
    geom_errorbarh(
      aes(xmin = lower, xmax = upper, y = level),
      height = 0.3) +
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

  widths <- unit.c(grobWidth(grobs[[1]]),
                  unit(c(14 * fontsize, 1, 50 * fontsize),
                       c("pt", "null", "pt")))
  heights <- unit.c(
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
