#' Adverse Event Category Plot
#'
#' Draw adverse event category plot.
#' @inheritParams argument_convention
#' @param id (`vector`)\cr contains subject identifier. Usually it is \code{ADAE$USUBJID}.
#' @param arm_sl (`vector`)\cr contains the subject level treatment variable.
#' For example, \code{ADSL$ACTARM}.
#' @param subgroups (`data.frame`)\cr Variables to conduct analysis.
#' @param subgroups_sl (`data.frame`)\cr Subject level variables to conduct analysis.
#' Usually from ADSL.
#' @param ref (`character`)\cr indicates the name of the reference arm. Default is the first
#' level of \code{arm}.
#' @param trt (`character`)\cr indicates the name of the treatment arm. Default is the second
#' level of \code{arm}.
#' @param indent (`numeric`)\cr non-negative integer where 0 means that the subgroup levels should not be indented
#' @param subgroups_levels (`list`)\cr A nested named list of variables to conduct analysis.
#' The names of the nested lists are used to show as the label.
#' The children lists should start with "Total" = variable label,
#' followed by labels for each level of said variable. See example for reference.
#' @param xmax (`numeric`)\cr maximum range for the x-axis.
#' x-axis range will be automatically assigned based on risk output when xmax is less than or equal to 0.
#' xmax is 0 by default
#' @param arm_n (`logical`)\cr whether to display the N in each arm.
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#'
#' @details there is no equivalent STREAM output
#'
#' @return grob object
#'
#' @export
#'
#' @examples
#' library(scda)
#' library(grid)
#'
#' cached_data <- synthetic_cdisc_data("latest")
#' ADAE <- cached_data$adae
#' ADSL <- cached_data$adsl
#'
#' id <- ADAE$USUBJID
#' arm <- ADAE$ACTARMCD
#' arm_sl <- as.character(ADSL$ACTARMCD)
#' subgroups_sl <- ADSL[, c("SEX", "RACE", "STRATA1")]
#' subgroups <- ADAE[, c("SEX", "RACE", "STRATA1")]
#' subgroups_levels <- list(
#'   RACE = list(
#'     "Total" = "Race",
#'     "AMERICAN INDIAN OR ALASKA NATIVE" = "American",
#'     "WHITE" = "White",
#'     "ASIAN" = "Asian",
#'     "BLACK OR AFRICAN AMERICAN" = "African"
#'   ),
#'   STRATA1 = list(
#'     "Total" = "Strata",
#'     "A" = "TypeA",
#'     "B" = "TypeB",
#'     "C" = "Typec"
#'   ),
#'   SEX = list(
#'     "Total" = "Sex",
#'     "M" = "Male",
#'     "F" = "Female"
#'   )
#' )
#' # Example 1
#' p1 <- g_ae_sub(id,
#'   arm,
#'   arm_sl,
#'   subgroups,
#'   subgroups_sl,
#'   trt = "ARM A",
#'   ref = "ARM C",
#'   subgroups_levels = subgroups_levels,
#'   arm_n = FALSE
#' )
#' grid::grid.newpage()
#'
#' # Example 2: display number of patients in each arm
#' p2 <- g_ae_sub(id,
#'   arm,
#'   arm_sl,
#'   subgroups,
#'   subgroups_sl,
#'   trt = "ARM A",
#'   ref = "ARM C",
#'   subgroups_levels = subgroups_levels,
#'   arm_n = TRUE
#' )
#' grid::grid.newpage()
#'
#' # Example 3: preprocess data to only include treatment and control arm patients
#' trt <- "ARM A"
#' ref <- "ARM C"
#' ADAE <- synthetic_cdisc_data("latest")$adae
#' ADSL <- synthetic_cdisc_data("latest")$adsl %>%
#'   filter(ACTARMCD %in% c(trt, ref))
#' id <- ADAE$USUBJID
#' arm <- ADAE$ACTARMCD
#' arm_sl <- as.character(ADSL$ACTARMCD)
#' subgroups_sl <- ADSL[, c("SEX", "RACE", "STRATA1")]
#' subgroups <- ADAE[, c("SEX", "RACE", "STRATA1")]
#' subgroups_levels <- list(
#'   RACE = list(
#'     "Total" = "Race",
#'     "AMERICAN INDIAN OR ALASKA NATIVE" = "American",
#'     "WHITE" = "White",
#'     "ASIAN" = "Asian",
#'     "BLACK OR AFRICAN AMERICAN" = "African"
#'   ),
#'   STRATA1 = list(
#'     "Total" = "Strata",
#'     "A" = "TypeA",
#'     "B" = "TypeB",
#'     "C" = "Typec"
#'   ),
#'   SEX = list(
#'     "Total" = "Sex",
#'     "M" = "Male",
#'     "F" = "Female"
#'   )
#' )
#' p3 <- g_ae_sub(id,
#'   arm,
#'   arm_sl,
#'   subgroups,
#'   subgroups_sl,
#'   trt,
#'   ref,
#'   subgroups_levels = subgroups_levels,
#'   arm_n = FALSE
#' )
g_ae_sub <- function(id,
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
                       "mn", "mee", "blj", "ha", "beal"
                     ),
                     fontsize = 4,
                     arm_n = FALSE,
                     draw = TRUE) {
  diff_ci_method <- match.arg(diff_ci_method)

  if (!is.null(subgroups_levels)) {
    labels <- unlist(subgroups_levels)
    label_df <-
      tibble(
        level = stringr::str_replace_all(names(labels), "\\.", "__"),
        label = labels
      ) %>%
      bind_rows(c(level = "TOTAL__Total", label = "Overall")) %>%
      mutate(
        indents = stringr::str_dup(" ", if_else(
          stringr::str_detect(.data$level, "__Total"), 0, indent
        )),
        # create label with indents if not total
        label = paste0(.data$indents, .data$label)
      )
  }
  stopifnot("invalid arguments: check that the length of id and arm are identical" = length(id) == length(arm))
  checkmate::assert_character(arm_sl, min.len = 2)
  stopifnot("invalid arguments: trt and ref need to be from arm" = all(c(trt, ref) %in% unique(arm)))
  checkmate::assert_number(conf_level, lower = 0.5, upper = 1)
  checkmate::assert_number(fontsize, lower = 0)
  checkmate::assert_data_frame(subgroups)
  stopifnot(
    "invalid argument: subgroups needs to be a data.frame with nrow = length(arm)" =
      nrow(subgroups) == length(arm)
  )
  checkmate::assert_data_frame(subgroups_sl)
  stopifnot(
    "invalid argument: subgroups_sl need to be a data.frame with nrow = length(arm_sl)" =
      nrow(subgroups_sl) == length(arm_sl)
  )
  checkmate::assert_number(indent, lower = 0, finite = TRUE)
  checkmate::assert_number(xmax)
  stopifnot(
    "invalid argument: please only use the subgroups column names as the lists names in subgroups_levels" =
      all(names(subgroups_levels) %in% names(lapply(subgroups, levels)))
  )

  stopifnot(
    "invalid argument: please only include levels in subgroups columns in the nested subgroups_levels" =
      all(unlist(lapply(names(subgroups_levels), function(level_name) {
        all(names(subgroups_levels[[level_name]]) %in% c("Total", levels(subgroups[, level_name] %>% dplyr::pull())))
      })))
  )

  checkmate::assert_flag(arm_n)

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

  # a wide data frame contains event counts by arm in each subgroup
  df <- cbind(id = id, arm = arm, subgroups) %>%
    filter(arm %in% c(ref, trt)) %>%
    unique() %>%
    tidyr::pivot_longer(
      names_to = "strata",
      cols = colnames(subgroups),
      values_to = "value"
    ) %>%
    group_by(arm, .data$strata, .data$value) %>%
    summarise(n = n()) %>%
    full_join(df_ref, by = "arm") %>%
    tidyr::replace_na(list(n = 0)) %>%
    ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c("strata", "value"),
      names_from = "arm",
      values_from = "n",
      names_prefix = "count__",
      values_fill = list(n = 0)
    )

  # a wide data frame contains total counts by arm in each subgroup
  df_sl <- cbind(arm = arm_sl, subgroups_sl) %>%
    filter(arm %in% c(ref, trt)) %>%
    mutate(TOTAL = "Total") %>%
    tidyr::pivot_longer(
      names_to = "strata",
      cols = c(colnames(subgroups_sl), "TOTAL"),
      values_to = "value"
    ) %>%
    group_by(arm, .data$strata, .data$value) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c("strata", "value"),
      names_from = "arm",
      values_from = "n",
      names_prefix = "total__",
      values_fill = list(n = 0)
    )

  # calculate the risk difference and risk difference CI in each subgroup
  df <- df %>%
    left_join(df_sl, by = c("strata", "value")) %>%
    group_by(.data$strata, .data$value) %>%
    mutate(
      !!r1 := !!x1 / !!n1,
      !!r2 := !!x2 / !!n2,
      lower = DescTools::BinomDiffCI(
        !!x1, !!n1, !!x2, !!n2,
        conf_level,
        method = diff_ci_method
      )[2],
      upper = DescTools::BinomDiffCI(
        !!x1, !!n1, !!x2, !!n2,
        conf_level,
        method = diff_ci_method
      )[3],
      riskdiff = !!r1 - !!r2
    ) %>%
    tidyr::pivot_longer(
      matches("__"),
      names_to = c(".value", "arm"),
      names_sep = "__"
    ) %>%
    ungroup() %>%
    tidyr::unite("level", "strata", "value", remove = FALSE, sep = "__")

  # create label for plotting
  level_format_df <- df %>%
    select("strata", "value") %>%
    unique() %>%
    group_by(.data$strata) %>%
    summarise(value = paste(c("Total", .data$value), collapse = ",")) %>%
    tidyr::separate_rows("value", sep = ",") %>%
    unique() %>%
    tidyr::unite("level", "strata", "value", sep = "__", remove = FALSE) %>%
    mutate(order = if_else(.data$strata == "TOTAL", integer(1), -row_number())) %>%
    arrange(order)

  if (is.null(subgroups_levels)) {
    level_format_df <- level_format_df %>%
      mutate(label = if_else(
        .data$strata == "TOTAL",
        "Overall",
        if_else(.data$value == "Total", .data$strata, paste0(stringr::str_c(
          rep(" ", indent),
          collapse = ""
        ), .data$value))
      ))
  } else {
    level_format_df <- level_format_df %>%
      inner_join(label_df, by = "level")
  }

  df <- df %>%
    semi_join(level_format_df, by = "level")

  mytheme <- theme_osprey(fontsize = fontsize, blank = TRUE)

  y_axis <-
    scale_y_discrete(
      limits = level_format_df$level,
      breaks = level_format_df$level,
      labels = level_format_df$label
    )

  df_risk <- df %>%
    select("level", "lower", "upper", "riskdiff") %>%
    unique()
  if (xmax <= 0) {
    xmax <- max(abs(df_risk$upper), abs(df_risk$lower), na.rm = TRUE)
  }
  p1 <-
    ggplot(df_risk) +
    geom_point(
      aes(x = .data$riskdiff, y = .data$level),
      size = fontsize
    ) +
    geom_vline(
      data = NULL,
      xintercept = 0,
      linetype = 1,
      color = "grey"
    ) +
    geom_errorbarh(
      aes(xmin = .data$lower, xmax = .data$upper, y = .data$level),
      height = 0.3
    ) +
    mytheme +
    theme(
      axis.ticks.x = element_line(),
      axis.line.x = element_line()
    ) +
    y_axis +
    coord_cartesian(xlim = c(-xmax, xmax))
  p1_grob <- ggplotGrob(p1)

  df_total <- df %>%
    group_by(.data$level) %>%
    summarise(n = sum(.data$total)) %>%
    mutate(percent = n / length(arm_sl) * 100)

  if (arm_n) {
    df_byarm <- df %>%
      group_by(.data$level, .data$arm) %>%
      summarise(n = sum(.data$total)) %>%
      tidyr::pivot_wider(names_from = arm, values_from = n) %>%
      rename(n_trt = matches(trt), n_ref = matches(ref))

    df_total <- df_total %>%
      left_join(df_byarm, by = "level") %>%
      mutate(
        percent_trt = .data$n_trt / .data$n * 100,
        percent_ref = .data$n_ref / .data$n * 100
      )
  }

  p2 <- ggplot(df_total) +
    geom_text(aes(
      x = "Patients(%)",
      y = .data$level,
      label = sprintf("%i (%.1f)", n, .data$percent)
    ), size = fontsize) +
    mytheme +
    y_axis +
    scale_x_discrete(position = "top")

  p2_grob <- ggplotGrob(p2)

  if (arm_n) {
    p2_trt <- ggplot(df_total) +
      geom_text(aes(
        x = "TRT",
        y = .data$level,
        label = sprintf("%i", .data$n_trt)
      ), size = fontsize) +
      mytheme +
      theme(axis.text.y = element_blank()) +
      y_axis +
      scale_x_discrete(position = "top")

    p2_ref <- ggplot(df_total) +
      geom_text(aes(
        x = "CONT",
        y = .data$level,
        label = sprintf("%i", .data$n_ref)
      ), size = fontsize) +
      mytheme +
      theme(axis.text.y = element_blank()) +
      y_axis +
      scale_x_discrete(position = "top")

    p2_trt_grob <- ggplotGrob(p2_trt)
    p2_ref_grob <- ggplotGrob(p2_ref)
  }

  df_ci <- df %>%
    tidyr::pivot_wider(
      names_from = "arm",
      values_from = "risk",
      id_cols = c("level", "lower", "upper", "riskdiff")
    ) %>%
    tidyr::pivot_longer(
      names_to = "x",
      values_to = "v",
      cols = c("lower", "upper", "riskdiff", matches(trt), matches(ref))
    ) %>%
    mutate(vlabel = sprintf("%.2f", .data$v))


  labels <- c("TRT", "CONT", "Diff", "Lower", "Upper")
  p3 <- ggplot(df_ci) +
    geom_text(aes(x = .data$x, y = .data$level, label = .data$vlabel), size = fontsize) +
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
  if (arm_n) {
    grobs <- append(grobs, grob_parts(p2_trt_grob, c("axis-t", "panel")))
    grobs <- append(grobs, grob_parts(p2_ref_grob, c("axis-t", "panel")))
  }
  grobs <- append(grobs, grob_parts(p1_grob, c("panel", "axis-b")))
  grobs <- append(grobs, grob_parts(p3_grob, c("axis-t", "panel")))
  less_risk <- grid::textGrob(
    "Favor\nTreatment",
    just = "centre",
    x = grid::unit(fontsize * .pt, "pt"),
    gp = grid::gpar(fontsize = fontsize * .pt, fontface = "bold")
  )
  more_risk <- grid::textGrob(
    "Favor\nControl",
    just = "centre",
    x = grid::unit(1, "npc") - grid::unit(fontsize * .pt, "pt"),
    gp = grid::gpar(fontsize = fontsize * .pt, fontface = "bold")
  )
  risk_label <- gridExtra::arrangeGrob(less_risk, more_risk, nrow = 1)
  grobs <- append(grobs, list(
    grid::textGrob(
      "Risk Difference (CI)",
      just = "centre",
      x = grid::unit(0.5, "npc"),
      y = grid::unit(0.5, "npc"),
      gp = grid::gpar(fontsize = fontsize * .pt, fontface = "bold")
    ),
    risk_label
  ))

  widths <- if (arm_n) {
    grid::unit.c(
      grid::grobWidth(grobs[[1]]),
      grid::unit(
        c(14 * fontsize, rep(10 * fontsize, 2), 1, 50 * fontsize),
        c(rep("pt", 3), "null", "pt")
      )
    )
  } else {
    grid::unit.c(
      grid::grobWidth(grobs[[1]]),
      grid::unit(
        c(14 * fontsize, 1, 50 * fontsize),
        c("pt", "null", "pt")
      )
    )
  }

  heights <- if (arm_n) {
    grid::unit.c(
      grid::grobHeight(grobs[[10]]),
      grid::unit(1, "null"),
      rep(grid::grobHeight(grobs[[9]]), 3),
      grid::unit(fontsize * .pt, "pt")
    )
  } else {
    grid::unit.c(
      grid::grobHeight(grobs[[6]]),
      grid::unit(1, "null"),
      grid::grobHeight(grobs[[5]]),
      grid::unit(fontsize * .pt * 3, "pt")
    )
  }

  boldfont <- grid::gpar(
    fontsize = fontsize * 4,
    fontface = "bold",
    lineheight = 1
  )
  layout_matrix <- if (arm_n) {
    rbind(
      c(NA, 2, 4, 6, 12, 10),
      c(1, 3, 5, 7, 8, 11),
      c(NA, NA, NA, NA, 9, NA),
      c(NA, NA, NA, NA, 13, NA)
    )
  } else {
    rbind(
      c(NA, 2, 8, 6),
      c(1, 3, 4, 7),
      c(NA, NA, 5, NA),
      c(NA, NA, 9, NA)
    )
  }

  ret <- gridExtra::arrangeGrob(
    grobs = grobs,
    layout_matrix = layout_matrix,
    heights = heights,
    widths = widths,
    clip = "on"
  )
  if (draw) {
    grid::grid.draw(ret)
  }
  invisible(ret)
}
