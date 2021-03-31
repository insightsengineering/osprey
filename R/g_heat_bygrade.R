#' Heatmap by Grade
#'
#' This function plots heatmap
#'
#' @param id_var (`character`)\cr name of the column that contains the unique subject identifier shared by all data
#' Usually it is \code{"USUBJID"}.
#' @param visit_var (`character`)\cr name of the column that contains the analysis visit. Usually it is \code{AVISIT}
#' @param exp_data (`dataframe`)\cr exposure data. Usually it is \code{ADEX}.
#' @param anno_data (`dataframe`)\cr annotation data contains subject level characteristics. Usually it is \code{ADSL}
#' @param anno_var (`vector`) a vector of columns names to include for the annotation
#' @param heat_data (`dataframe`)\cr contains the information needed for the text over heatmap
#' Usually is \code{ADCM}.
#' @param heat_color_var (`character`)\cr
#' @param heat_color_opt (`vector`)\cr
#' @param conmed_data (`dataframe`)\cr default is \code{NULL} (no conmed plotted)
#' @param conmed_var (`character`)\cr default is \code{NULL} (no conmed plotted)
#' @param conmed_opt (`vector`)\cr default is \code{NULL} (use default color or no conmed plotted)
#' @param xlab (`character`)\cr
#' @param title (`character`)\cr
#' @import ggplot2
#' @importFrom stringr str_to_upper
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
#' library(random.cdisc.data)
#' ADSL <- radsl(N = 30)
#' ADEX <- radex(ADSL)
#' ADAE <- radae(ADSL)
#' ADCM <- radcm(ADSL)
#' # add AVISIT in ADAE
#' ADAE$AVISIT <- sample(unique(ADEX$AVISIT[!is.na(ADEX$AVISIT)]), nrow(ADAE), TRUE)
#' exp_data <- ADEX %>%
#'   filter(PARCAT1 == "INDIVIDUAL") %>%
#'   group_by(USUBJID) %>%
#'   # create a shorter subject identifier
#'   mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1)) %>%
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
#'   )
#'
#' # include three conmed levels
#' ADCM_labs <- rtables::var_labels(ADCM)
#' ADCM <- ADCM %>%
#'   filter(
#'     CMDECOD == "medname A_1/3" | CMDECOD == "medname A_2/3" | CMDECOD == "medname A_3/3"
#'     ) %>%
#'   mutate(CMDECOD = factor(CMDECOD, levels = unique(CMDECOD)))
#' rtables::var_labels(ADCM) <- ADCM_labs
#' conmed_data <- ADCM %>%
#'   group_by(USUBJID) %>%
#'   mutate(SUBJ = utils::tail(strsplit(USUBJID, "-")[[1]], n = 1))
#'
#' g_heat_bygrade(
#'   id_var = "SUBJ",
#'   visit_var = "AVISIT",
#'   exp_data,
#'   anno_data,
#'   anno_var = names(anno_data),
#'   heat_data,
#'   heat_color_var = "AETOXGR",
#'   heat_color_opt,
#'   conmed_data,
#'   conmed_var = "CMDECOD",
#'   conmed_opt = c("green", "green3", "green4")
#'   )
#'
#' g_heat_bygrade(
#'   id_var = "SUBJ",
#'   visit_var = "AVISIT",
#'   exp_data,
#'   anno_data,
#'   anno_var = names(anno_data),
#'   heat_data,
#'   heat_color_var = "AETOXGR",
#'   heat_color_opt
#'   )
g_heat_bygrade <- function(id_var,
                           visit_var,
                           exp_data,
                           anno_data,
                           anno_var,
                           heat_data,
                           heat_color_var,
                           heat_color_opt = NULL,
                           conmed_data = NULL,
                           conmed_var = NULL,
                           conmed_opt = NULL,
                           xlab = "Visit",
                           title = "Heatmap by Grade") {
  # check if all PARCAT1 in exp_data is "individual"
  stop_if_not(
    list(is.data.frame(exp_data)),
    list(!is.na(exp_data[[visit_var]]), "invalid argument: please only include 'INDIVIDUAL' record in exp_data"),
    list(is.data.frame(anno_data)),
    list(
      length(anno_var) <= 3,
      "invalid argument: anno_data can only contain 3 or less columns including subject ID"
      ),
    list(is.data.frame(heat_data)),
    list(
      is_character_single(heat_color_var) && heat_color_var %in% names(heat_data),
      "invalid argument: heat_color_var should be a varaible name in heat_data"
      ),
    list(
      any(!is.null(conmed_data), is.null(conmed_data) == is.null(conmed_var)),
      "invalid argument: need to provide conmed_data and conmed_var"
      ),
    list(
      is.null(conmed_var) | length(levels(conmed_data[[conmed_var]])) <= 3,
      "invalid argument: please only include no more than three conmeds for plotting"
      ),
    list(
      table(c(names(exp_data), names(anno_data), names(heat_data), names(conmed_data)))[id_var] == 4,
      "Please include a column named id_var in exp_data, anno_data, heat_data, and conmed_data"
    ),
    list(
      table(c(names(exp_data), names(heat_data), names(conmed_data)))[visit_var] == 3,
      "Please include a column named visit_var in exp_data, heat_data, and conmed_data"
    )
  )

  anl_data <- exp_data %>%
    select(!!id_var, !!sym(visit_var)) %>%
    left_join(heat_data, by = c(id_var, visit_var)) %>%
    distinct() %>%
    mutate(heat_color_num = tidyr::replace_na(as.numeric(.data[[heat_color_var]]), 0)) %>%
    group_by(!!sym(id_var), !!sym(visit_var)) %>%
    arrange(!!sym(visit_var)) %>%
    mutate(heat_color_max = factor(max(.data$heat_color_num))) %>%
    select(- (!!heat_color_var), -.data$heat_color_num) %>%
    distinct() %>%
    left_join(anno_data, by = id_var)
  #dose reduction data
  ex_red <- exp_data %>%
    filter(.data$PARAMCD == "DOSE") %>%
    group_by(!!sym(id_var)) %>%
    arrange(.data$ASTDTM) %>%
    mutate(
      RANK = order(.data$ASTDTM),
      LASTDOSE = lag(.data$AVAL),
      DOSERED = ifelse(.data$RANK != 1 & .data$AVAL < .data$LASTDOSE, "Y", "")
      ) %>%
    select(!!sym(id_var), !!sym(visit_var), .data$RANK, .data$AVAL, .data$LASTDOSE, .data$DOSERED) %>%
    filter(.data$DOSERED == "Y")
  # does ongoing data
  exp_lst <- exp_data %>%
    filter(.data$PARAMCD == "DOSE") %>%
    filter(str_to_upper(.data$EOSSTT) == "ONGOING") %>%
    arrange(!!sym(visit_var)) %>%
    slice_tail() %>%
    select(!!sym(id_var), !!sym(visit_var))

  visit_levels <- unique(anl_data[[visit_var]])
  if (!is.null(conmed_data) & !is.null(conmed_var)) {
    conmed_data <- conmed_data %>%
      left_join(anl_data, by = id_var) %>%
      ungroup() %>%
      mutate(
        conmed_num = as.numeric(.data$CMDECOD),
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
  levels(anl_data$heat_color_max) <- sort(as.numeric(levels(anl_data$heat_color_max)))
  levels(anl_data$heat_color_max)[levels(anl_data$heat_color_max) == "0"] <- "No Event"

  p <- ggplot(
    data = anl_data,
    aes(x = !!sym(visit_var), y = factor(!!sym(id_var), levels = c(subj_levels, "")))
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
        y = as.numeric(factor(!!sym(id_var), levels = subj_levels)) + 0.3,
        x = as.numeric(factor(!!sym(visit_var), levels = visit_levels)),
        yend = as.numeric(factor(!!sym(id_var), levels = subj_levels)) - 0.3,
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
        y = as.numeric(factor(!!sym(id_var), levels = subj_levels)),
        yend = as.numeric(factor(!!sym(id_var), levels = subj_levels))
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
          values = if (!is.null(conmed_opt)) conmed_opt else rep("black", 5)
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

  #plot left legend
  t <- anl_data %>%
    as.data.frame() %>%
    select((!!anno_var)) %>%
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
