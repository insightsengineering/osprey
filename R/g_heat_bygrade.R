#' Heatmap by Grade
#'
#' This function plots heatmap
#'
#' @param id_var (`character`)\cr name of the column that contains the unique subject identifier shared by all data
#' Usually it is \code{"USUBJID"}.
#' @param exp_data (`data.frame`)\cr exposure data. Usually it is \code{ADEX}.
#' @param visit_var (`character`)\cr name of the column that contains the analysis visit. Usually it is \code{"AVISIT"}
#' @param ongo_var (`character`)\cr name of the column in \code{exp_data} that contains the logical variable
#' indicating whether the treatment is still ongoing.
#' Usually it can be derived from \code{EOSSTT}
#' @param anno_data (`data.frame`)\cr annotation data that contains subject level characteristics.
#' Usually it is \code{ADSL}
#' @param anno_var (`character`) a vector of columns name(s) to include for the annotation
#' @param heat_data (`data.frame`)\cr data frame that contains the information needed for the text over heatmap
#' Usually it is \code{ADCM}.
#' @param heat_color_var (`character`)\cr name of the column that contains the heat grade
#' @param heat_color_opt optional, (`character`)\cr a named vector that maps the names to heat colors
#' @param conmed_data optional, (`data.frame`)\cr concomitant medicine data. Usually it is \code{ADCM}
#' default is \code{NULL} (no conmed plotted)
#' @param conmed_var optional, (`character`)\cr concomitant medicine variable name. Must be a column name in conmed_data
#' when conmed_data is provided. default is \code{NULL} (no conmed plotted)
#' @param conmed_color_opt optional, (`character`)\cr vector of color name(s) to conmed_data
#' @param xlab optional, (`character`)\cr string to be shown as x-axis label, default is \code{"Visit"}
#' @param title (`character`)\cr string to be shown as title of the plot.
#' default is \code{NULL} (no plot title is displayed)
#' @import ggplot2
#' @importFrom tidyr replace_na
#' @importFrom stats median
#' @importFrom grDevices terrain.colors
#'
#' @export
#'
#' @author Nina Qi (qit3) \email{qit3@gene.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' ADSL <- synthetic_cdisc_data("latest")$adsl %>% slice(1:30)
#' ADEX <- synthetic_cdisc_data("latest")$adex %>% filter(USUBJID %in% ADSL$USUBJID)
#' ADAE <- synthetic_cdisc_data("latest")$adae %>% filter(USUBJID %in% ADSL$USUBJID)
#' ADCM <- synthetic_cdisc_data("latest")$adcm %>% filter(USUBJID %in% ADSL$USUBJID)
#' # function to derive AVISIT from ADEX
#' add_visit <- function(data_need_visit) {
#'   visit_dates <- ADEX %>%
#'     filter(PARAMCD == "DOSE") %>%
#'     distinct(USUBJID, AVISIT, ASTDTM) %>%
#'     group_by(USUBJID) %>%
#'     arrange(ASTDTM) %>%
#'     mutate(next_vis = lead(ASTDTM), is_last = ifelse(is.na(next_vis), TRUE, FALSE)) %>%
#'     rename(this_vis = ASTDTM)
#'   data_visit <- data_need_visit %>%
#'     select(USUBJID, ASTDTM) %>%
#'     left_join(visit_dates, by = "USUBJID") %>%
#'     filter(ASTDTM > this_vis & (ASTDTM < next_vis | is_last == TRUE)) %>%
#'     left_join(data_need_visit)
#'   return(data_visit)
#' }
#' # add AVISIT in ADAE and ADCM
#' ADAE <- add_visit(ADAE)
#' ADCM <- add_visit(ADCM)
#' exp_data <- ADEX %>%
#'   filter(PARCAT1 == "INDIVIDUAL") %>%
#'   group_by(USUBJID) %>%
#'   # create a shorter subject identifier
#'   mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1)) %>%
#'   mutate(ongo_var = (EOSSTT == "ONGOING")) %>%
#'   ungroup()
#' anno_data <- ADSL %>%
#'   select(SEX, COUNTRY, USUBJID) %>%
#'   group_by(USUBJID) %>%
#'   mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1)) %>%
#'   ungroup() %>%
#'   select(-USUBJID)
#'
#' heat_data <- ADAE %>%
#'   select(USUBJID, AVISIT, AETOXGR) %>%
#'   group_by(USUBJID) %>%
#'   mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1)) %>%
#'   ungroup() %>%
#'   select(-USUBJID)
#' heat_color_opt <- c(
#'   "No Event" = "gray90",
#'   "1" = "lightsteelblue1",
#'   "2" = "steelblue1",
#'   "3" = "steelblue4",
#'   "4" = "maroon",
#'   "5" = "brown4"
#' )
#' ADCM_lab <- rtables::var_labels(ADCM)
#' ADCM <- ADCM %>%
#'   filter(
#'     CMDECOD == "medname A_1/3" | CMDECOD == "medname A_2/3" | CMDECOD == "medname A_3/3"
#'   ) %>%
#'   mutate(CMDECOD = factor(CMDECOD, levels = unique(CMDECOD)))
#' rtables::var_labels(ADCM) <- ADCM_lab
#' conmed_data <- ADCM %>%
#'   group_by(USUBJID) %>%
#'   mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1))
#' # example plotting conmed
#' g_heat_bygrade(
#'   id_var = "SUBJ",
#'   exp_data,
#'   visit_var = "AVISIT",
#'   ongo_var = "ongo_var",
#'   anno_data,
#'   anno_var = c("SEX", "COUNTRY"),
#'   heat_data,
#'   heat_color_var = "AETOXGR",
#'   heat_color_opt,
#'   conmed_data,
#'   conmed_var = "CMDECOD",
#'   conmed_color_opt = c("green", "green3", "green4")
#' )
#'
#' # example not plotting conmed
#' g_heat_bygrade(
#'   id_var = "SUBJ",
#'   exp_data,
#'   visit_var = "AVISIT",
#'   ongo_var = "ongo_var",
#'   anno_data,
#'   anno_var = c("SEX", "COUNTRY"),
#'   heat_data,
#'   heat_color_var = "AETOXGR",
#'   heat_color_opt
#' )
g_heat_bygrade <- function(id_var,
                           exp_data,
                           visit_var,
                           ongo_var,
                           anno_data,
                           anno_var,
                           heat_data,
                           heat_color_var,
                           heat_color_opt = NULL,
                           conmed_data = NULL,
                           conmed_var = NULL,
                           conmed_color_opt = NULL,
                           xlab = "Visit",
                           title = NULL) {
  # check if all PARCAT1 in exp_data is "individual"
  checkmate::assert_string(id_var)
  checkmate::assert_data_frame(exp_data)
  stopifnot(
    "invalid argument: please only include 'INDIVIDUAL' record in exp_data" = !is.na(exp_data[[visit_var]])
  )
  checkmate::assert_string(visit_var)
  checkmate::assert_string(ongo_var)
  checkmate::assert(
    checkmate::check_subset(ongo_var, names(exp_data)),
    checkmate::check_logical(exp_data[[ongo_var]])
  )
  checkmate::assert_data_frame(anno_data)
  stopifnot(
    "invalid argument: anno_data can only contain 3 or less columns including subject ID" = length(anno_var) <= 2
  )
  checkmate::assert_data_frame(heat_data)
  checkmate::assert(
    checkmate::test_string(heat_color_var),
    checkmate::test_subset(heat_color_var, names(heat_data))
  )
  checkmate::assert_string(conmed_var, null.ok = TRUE)
  checkmate::assert_subset(conmed_var, names(conmed_data))
  stopifnot(
    "invalid argument: need to provide conmed_data and conmed_var" =
    any(is.null(conmed_data), is.null(conmed_data) == is.null(conmed_var))
  )
  stopifnot(
    "invalid argument: please only include no more than three conmeds for plotting" =
      is.null(conmed_var) || length(levels(conmed_data[[conmed_var]])) <= 3
  )
  stopifnot(
    "invalid argument: please specify conmed_color_opt for all unique conmed_var" =
    is.null(conmed_data) || is.null(conmed_color_opt) ||
      length(conmed_color_opt) == length(unique(conmed_data[[conmed_var]]))
  )
  if (!((is.null(conmed_data) && table(c(names(exp_data), names(anno_data), names(heat_data)))[id_var] == 3) ||
      table(c(names(exp_data), names(anno_data), names(heat_data), names(conmed_data)))[id_var] == 4)) {
    stop(
      paste(
        "exp_data, anno_data, heat_data, and conmed_data (if plotting conmed) must include a column named",
        id_var,
        sep = " "
      )
    )
  }

  if (!((is.null(conmed_data) && table(c(names(exp_data), names(heat_data)))[visit_var] == 2) ||
      table(c(names(exp_data), names(heat_data), names(conmed_data)))[visit_var] == 3)) {
    stop(
      paste(
        "exp_data, anno_data, heat_data, and conmed_data (if plotting conmed) must include a column named",
        visit_var,
        sep = " "
      )
    )
  }

  anl_data <- exp_data %>%
    select(!!id_var, !!sym(visit_var)) %>%
    left_join(heat_data, by = c(id_var, visit_var)) %>%
    distinct() %>%
    mutate(heat_color_num = tidyr::replace_na(as.numeric(.data[[heat_color_var]]), 0)) %>%
    group_by(!!sym(id_var), !!sym(visit_var)) %>%
    arrange(!!sym(visit_var)) %>%
    mutate(heat_color_max = factor(max(.data$heat_color_num), levels = 0:5)) %>%
    select(-(!!heat_color_var), -.data$heat_color_num) %>% # nolint
    distinct() %>%
    left_join(anno_data, by = id_var)

  # dose reduction data
  ex_red <- exp_data %>%
    filter(.data$PARAMCD == "DOSE") %>%
    group_by(!!sym(id_var)) %>%
    arrange(.data$ASTDTM) %>%
    mutate(
      RANK = order(.data$ASTDTM),
      LASTDOSE = lag(.data$AVAL),
      DOSERED = ifelse(.data$RANK != 1 & .data$AVAL < .data$LASTDOSE, TRUE, FALSE)
    ) %>%
    select(!!sym(id_var), !!sym(visit_var), .data$RANK, .data$AVAL, .data$LASTDOSE, .data$DOSERED) %>%
    filter(.data$DOSERED == TRUE)
  # does ongoing data
  exp_lst <- exp_data %>%
    filter(.data$PARAMCD == "DOSE") %>%
    filter(!!sym(ongo_var) == TRUE) %>%
    group_by(!!sym(id_var)) %>%
    arrange(!!sym(visit_var)) %>%
    slice_tail() %>%
    select(!!sym(id_var), !!sym(visit_var))
  visit_levels <- unique(anl_data[[visit_var]])
  if (!is.null(conmed_data) & !is.null(conmed_var)) {
    conmed_data <- conmed_data %>%
      left_join(anl_data, by = c(id_var, visit_var)) %>%
      ungroup() %>%
      mutate(
        conmed_num = as.numeric(.data[[conmed_var]]),
        conmed_num_m = median(unique(.data$conmed_num), na.rm = TRUE)
      ) %>%
      mutate(
        distance = (ifelse(
          .data$conmed_num <= .data$conmed_num_m,
          .data$conmed_num - 1,
          .data$conmed_num + 1
        ) - .data$conmed_num_m) / 5,
        conmed_x = as.numeric(!!sym(visit_var)) + .data$distance
      )
  }
  subj_levels <- unique(anl_data[[id_var]])
  levels(anl_data$heat_color_max)[levels(anl_data$heat_color_max) == "0"] <- "No Event"
  p <- ggplot(
    data = anl_data,
    aes(x = !!sym(visit_var), y = factor(!!sym(id_var), levels = c(rev(subj_levels), "")))
  ) +
    geom_tile(aes(fill = .data$heat_color_max)) +
    scale_y_discrete(drop = FALSE) +
    scale_fill_manual(
      name = "Highest grade of\nindividual events",
      values = if (!is.null(heat_color_opt)) heat_color_opt else rev(terrain.colors(6))
    ) +
    # plot dose reduction
    geom_segment(
      data = ex_red,
      aes(
        y = as.numeric(factor(!!sym(id_var), levels = rev(subj_levels))) + 0.3,
        x = as.numeric(factor(!!sym(visit_var), levels = visit_levels)),
        yend = as.numeric(factor(!!sym(id_var), levels = rev(subj_levels))) - 0.3,
        xend = as.numeric(factor(!!sym(visit_var), levels = visit_levels))
      ),
      arrow = arrow(length = unit(0.1, "cm")),
      size = .5,
      color = "black"
    ) +
    # plot ongoing
    geom_segment(
      data = exp_lst,
      aes(
        x = as.numeric(factor(!!sym(visit_var), levels = visit_levels)) + 0.5,
        xend = as.numeric(factor(!!sym(visit_var), levels = visit_levels)) + 0.65,
        y = as.numeric(factor(!!sym(id_var), levels = rev(subj_levels))),
        yend = as.numeric(factor(!!sym(id_var), levels = rev(subj_levels)))
      ),
      arrow = arrow(length = unit(0.1, "cm")),
      size = .5,
      color = "black"
    )
  if (!is.null(conmed_data) & !is.null(conmed_var)) {
    p <- p +
      geom_point(
        data = conmed_data,
        aes(
          x = .data$conmed_x,
          y = as.numeric(factor(!!sym(id_var), levels = rev(subj_levels))),
          shape = .data[[conmed_var]],
          color = .data[[conmed_var]]
        ),
        size = 2
      ) +
      scale_colour_manual(
        name = rtables::var_labels(conmed_data)[conmed_var],
        values = if (!is.null(conmed_color_opt)) conmed_color_opt else rep("black", 5)
      ) +
      scale_shape_manual(
        name = rtables::var_labels(conmed_data)[conmed_var],
        values = c(15:17)
      )
  }

  p <- p +
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

  # plot left legend
  t <- anl_data %>%
    as.data.frame() %>%
    select((!!anno_var), (!!id_var)) %>%
    distinct()
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
