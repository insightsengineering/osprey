
#' Swimlane Plot
#'
#' Swimlane plot is often used in Early Development (ED) and displays individual
#' patient bar plot with markers of events and patient level annotation
#'
#' @param bar_id vector of IDs to identify each bar
#' @param bar_length numeric vector to be plotted as length for each bar
#' @param sort_by vector to sort bars
#' @param col_by vector to color bars
#' @param marker_id vector of IDs to identify markers within each bar. Default is the same as bar_id.
#' @param marker_pos numeric vector to specify position for each marker point
#' @param marker_shape vector to specify shape for markers
#' @param marker_shape_opt aesthetic values to map shape values (named vector to map shape values to each name)
#' @param marker_color vector to specify color for markers
#' @param marker_color_opt aesthetic values to map shape values (named vector to map shape values to each name)
#' @param anno_txt dataframe of subject-level variables to be displayed as annotation on the left
#' @param yref_line numeric vector to plot reference lines
#' @param ytick_at optional break interval of bar length axis
#' @param ylab label for bar length
#' @param title string to be displayed as plot title
#'
#' @template author_qit3
#'
#' @import ggplot2 grid gridExtra gtable
#'
#' @return plot object
#'
#' @export
#'
#' @examples
#' # Example 1
#' library(random.cdisc.data)
#' library(dplyr)
#' ASL <- radam("ASL", N=50, start_with = list(TRTDUR = rexp(50, 1/100)))
#' ARS <- radam("ARS", ADSL = ASL,
#' start_with = list(
#'       ADY = rexp(nrow(ASL), 1/80)
#'    )) %>% filter(PARAMCD == "OVRINV")
#' ANL <- ASL %>% left_join(ARS, by = c("STUDYID", "USUBJID"))
#' anno_txt <- ASL[, c("ARMCD", "SEX")]
#'
#' g_swimlane(bar_id = ASL$USUBJID,
#' bar_length = ASL$TRTDUR,
#' sort_by = ASL$ARM,
#' col_by = ASL$ARM,
#' marker_id = ANL$USUBJID,
#' marker_pos = ANL$ADY,
#' marker_shape = ANL$AVALC,
#' marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
#' marker_color = NULL,
#' marker_color_opt = NULL,
#' anno_txt = anno_txt,
#' yref_line = c(100, 200),
#' ytick_at = waiver(),
#' ylab = "Time from First Treatment (Day)",
#' title = "Swimlane Plot")
#'
#' # Example 2
#' library(dplyr)
#' ASL <- rADSL
#' ARS <- rADRS
#'
#' anno_txt_vars <- c("ARMCD", "SEX", "COUNTRY")
#' anno_txt <- ASL[, anno_txt_vars]
#'
#' # markers from ARS
#' ARS <- ASL %>% select(USUBJID) %>%
#' left_join(ARS, "USUBJID") %>%
#' filter(PARAMCD == "OVRINV") %>%
#' select(USUBJID, ADY, AVALC)
#'
#' # markers from ASL - discontinuation
#' ADS <- ASL %>%
#' filter(EOSSTT == "Discontinued" | DCSREAS != "") %>%
#' select(USUBJID, EOSDY, DCSREAS) %>%
#' dplyr::rename(ADY = EOSDY, AVALC = DCSREAS)
#'
#' # combine ARS with ADS records as one data for markers and join with ASL
#' ANL <- ASL %>%
#' inner_join(rbind(ARS, ADS), "USUBJID")
#'
#' g_swimlane(bar_id = ASL$USUBJID,
#' bar_length = ASL$TRTDURD,
#' sort_by = NULL,
#' col_by = ASL$ARMCD,
#' marker_id = ANL$USUBJID,
#' marker_pos = ANL$ADY,
#' marker_shape = ANL$AVALC,
#' marker_shape_opt <- c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 0,
#' "Adverse Event" = 7, "Death" = 8, "Physician Decision" = 9, "Progressive Disease" = 10,
#' "Symptomatic Deterioation" = 11, "Withdrawal by Subject" = 12),
#' marker_color = ANL$AVALC,
#' marker_color_opt <- c("CR" = "green", "PR" = "blue", "SD" = "yellow", "PD" = "red",
#' "NE" = "grey", "Adverse Event" = "orange", "Death" = "black", "Physician Decision" = "navy",
#' "Progressive Disease" = "purple", "Symptomatic Deterioation" = "cyan",
#' "Withdrawal by Subject" = "darkred"),
#' anno_txt = anno_txt,
#' yref_line = c(50, 100),
#' ytick_at = waiver(),
#' ylab = "Time from First Treatment (Day)",
#' title = "Swimlane Plot")
#'

g_swimlane <- function(bar_id,
                       bar_length,
                       sort_by = NULL,
                       col_by = NULL,
                       marker_id = NULL,
                       marker_pos = NULL,
                       marker_shape = NULL,
                       marker_shape_opt = NULL,
                       marker_color = NULL,
                       marker_color_opt = NULL,
                       anno_txt = NULL,
                       yref_line = NULL,
                       ytick_at = waiver(),
                       ylab,
                       title
) {

  # check data
  if (!is.null(sort_by)) check_same_N(bar_id = bar_id, bar_length = bar_length, sort_by = sort_by)
  if (!is.null(col_by)) check_same_N(bar_id = bar_id, bar_length = bar_length, col_by = col_by)

  if (!is.null(marker_id) & length(which(!marker_id %in% bar_id)) > 0) stop("marker_id ", marker_id[which(!marker_id %in% bar_id)], " is not in bar_id")

  if (!is.null(marker_id) & !is.null(marker_pos)) check_same_N(marker_id = marker_id, marker_pos = marker_pos)
  if (!is.null(marker_id) & !is.null(marker_shape)) check_same_N(marker_id = marker_id, marker_shape = marker_shape)
  if (!is.null(marker_id) & !is.null(marker_color)) check_same_N(marker_id = marker_id, marker_color = marker_color)

  # data for plot
  bar_data <- data.frame(
    bar_id,
    bar_length,
    sort_by = if (is.null(sort_by)) "x" else to_n(sort_by, length(bar_length)),
    col_by = if (is.null(col_by)) "x" else to_n(col_by, length(bar_length))
    )

  #data for marker
  if (is.null(marker_id)) marker_id <- bar_id
  marker_data <- data.frame(
    marker_id,
    marker_pos = if (is.null(marker_pos)) "x" else to_n(marker_pos, length(marker_id)),
    marker_shape = if (is.null(marker_shape)) "x" else to_n(marker_shape, length(marker_id)),
    marker_color = if (is.null(marker_color)) "x" else to_n(marker_color, length(marker_id))
    )

  # if sort by a variable, reorder bar_id by sort var and then bar length; otherwise sort by bar length
  if (!is.null(sort_by)) {
    bar_data$bar_id = factor(bar_data$bar_id,
                             levels = rev(unique(bar_data$bar_id[order(bar_data$sort_by, -bar_data$bar_length)])))
  } else{
    bar_data$bar_id = factor(bar_data$bar_id,
                             levels = rev(unique(bar_data$bar_id[order(-bar_data$bar_length)])))
  }

  # labeling
  ylabel <- deparse(substitute(bar_length))
  ylab <- if (is.null(ylab)) ylabel else ylab

  # plot bar plot first
  p <- ggplot(data = bar_data, aes(x = bar_id, y = bar_length)) +
    geom_bar(stat = "identity", aes(fill = col_by)) +
    coord_flip(xlim = c(1,length(unique(bar_id)) + 1)) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    ) +
    ylab(ylab)

  if (is.null(col_by)) {
    p <- p + guides(fill = FALSE)
  } else {
    p <- p + guides(fill = guide_legend("Bar Color", order = 1, ncol = 1)) +
      theme(
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.key = element_rect(fill = NA))
  }


  # plot marker
  if (!is.null(marker_pos)) {
    p <- p + geom_point(data = marker_data,
                        aes(x = marker_id, y = marker_pos, shape = marker_shape, color = marker_color), size = 2.5, na.rm = T) +
      scale_y_continuous(limits = c(0, max(bar_length, marker_pos) + 5), breaks = ytick_at, expand = c(0, 0))

    if (!is.null(marker_shape)) {
      p <- p + guides(shape = guide_legend("Marker Shape", order = 2))
    } else {
      p <- p + guides(shape = FALSE)
    }

    if (!is.null(marker_color)) {
      p <- p + guides(color = guide_legend("Marker Color", order = 3))
    } else {
      p <- p + guides(color = FALSE)
    }

    if (!is.null(marker_shape_opt)) {
      p <- p + scale_shape_manual(name = "Marker Shape",
                                  breaks = marker_data$marker_shape,
                                  values = marker_shape_opt)
    } else{
      p <- p + scale_shape_manual(name = "Marker Shape",
                                  breaks = marker_data$marker_shape,
                                  values = c(15:25, 0:14))
    }

    if (!is.null(marker_color_opt)) {
      p <- p + scale_color_manual(name = "Marker Color",
                                  breaks = marker_data$marker_color,
                                  values = marker_color_opt)
    } else{
      p <- p + scale_color_manual(name = "Marker Shape",
                                  breaks = marker_data$marker_color,
                                  values = c(1:25))
    }

  }


  # plot reference lines
  if (!is.null(yref_line)) {
    p <- p + geom_hline(yintercept = yref_line, linetype = "dashed", color = "red")
  }

  # plot title and labels
  if (!is.null(title)) {
    p <- p +
      labs(title = title) +
      theme(plot.title = element_text(face = "bold"))
  }


  # create annotation as a separate table plot
  if (is.null(anno_txt)) {
    t <- data.frame(bar_id, bar_length,
                    sort_by = if (is.null(sort_by)) "x" else to_n(sort_by, length(bar_length)))
  } else {
    t <- data.frame(bar_id, bar_length,
                    sort_by = if (is.null(sort_by)) "x" else to_n(sort_by, length(bar_length)),
                    anno_txt)
  }

  # if sort by a variable, reorder bar_id; otherwise sort by bar length
  if (!is.null(sort_by)) {
    t <- t[with(t, order(sort_by, -bar_length, bar_id)), -c(2,3)]
  } else{
    t <- t[with(t, order(-bar_length, bar_id)), -c(2,3)]
  }

  t <- as.data.frame(t)
  colnames(t)[1] <- " "

  my_theme <- ttheme_default(
    core = list(bg_params = list(fill = NA, col = NA),
                fg_params = list(cex = 0.8)),
    colhead = list(bg_params = list(fill = NA, col = NA),
                   fg_params = list(cex = 0.8)))
  tb <- tableGrob(t, rows = NULL, theme = my_theme)
  tb$heights <- unit(rep(1/nrow(tb), nrow(tb)), "null")

  # grab plot and table as one plot
  g0 <- ggplotGrob(p)
  g1 <- gtable_add_cols(g0, sum(tb$widths), 0)
  g <- gtable_add_grob(g1, tb, t = g1$layout[g1$layout$name == "panel", 1], l = 1)

  grid.newpage()
  grid.draw(g)

}

