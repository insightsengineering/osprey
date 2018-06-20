#' Swimlane bar plot for each id with markers
#'
#' @param bar_id vector of IDs to identify each bar
#' @param bar_length numeric vector to be plotted as length for each bar
#' @param sort_by vector to sort bars
#' @param col_by vector to color bars
#' @param marker_id vector of IDs to identify markers within each bar. Default is the same as bar_id.
#' @param marker_pos numeric vector to specify position for each marker
#' @param marker_shape vector to specify shape for markers
#' @param marker_shape_opt aesthetic values to map shape values (named vector to map shape values to each name)
#' @param marker_color vector to specify color for markers
#' @param marker_color_opt aesthetic values to map shape values (named vector to map shape values to each name)
#' @param anno_txt dataframe of variables to be displayed as annotation on the left
#' @param yref_line numeric vector to plot reference lines
#' @param ytick_at break interval of y-axis
# #' @param xlab x label
#' @param ylab y label
#' @param title String to be displayed as plot title
#'
#' @author qit3
#'
#' @import ggplot2 grid gridExtra gtable
#'
#' @return plot object
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' ASL <- radam("ASL", N=50, start_with = list(TRTDUR = rexp(50, 1/100)))
#' ARS <- radam("ARS", ADSL = ASL,
#' start_with = list(
#'       ADY = rexp(nrow(ASL), 1/80)
#'    )) %>% filter(PARAMCD == "OVRINV")
#' ANL <- ASL %>% left_join(ARS, by = c("STUDYID", "USUBJID"))
#' # pre-process bar length breaks for treatment duration for longitudinal records
#' anno_txt <- ASL[, c("ARMCD", "SEX", "RACE")]
#'
#' g_swimlane(bar_id = ASL$USUBJID,
#' bar_length = ASL$TRTDUR,
#' sort_by = ANL$ARM,
#' col_by = ANL$ARM,
#' marker_id = ANL$USUBJID,
#' marker_pos = ANL$ADY,
#' marker_shape = ANL$AVALC,
#' marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
#' marker_color = NULL,
#' marker_color_opt = NULL,
#' anno_txt = anno_txt,
#' yref_line = c(100, 200),
#' ytick_at = waiver(),
#' # xlab = "Patient ID",
#' ylab = "Time from First Treatment (Day)",
#' title = "Swimlane Plot")
#'
#' \donotrun{
#' library(dplyr)
#'
#' atx <- read.bce("/opt/BIOSTAT/qa/s30103j/libraries/atx.sas7bdat")
#' asl <- read.bce("/opt/BIOSTAT/qa/s30103j/libraries/asl.sas7bdat")
#' xars <- read.bce("/opt/BIOSTAT/qa/s30103j/libraries/xars.sas7bdat")
#' # phase 1A data
#' # pre-process ASl and ARS to fit in ANL
#' ATX <- atx %>% filter(grepl("1a", APERIDC2))
#' ASL <- ATX %>% select(USUBJID) %>%
#' left_join(asl, by = "USUBJID") %>%
#' filter(SAFFL == "Y")
#'
#' anno_txt_var <- c("ARMCD", "SEX", "CADX")
#' anno_txt <- ASL[, anno_txt_var]
#'
#' ARS <- ASL %>% select(USUBJID) %>%
#' left_join(xars %>% filter(grepl("1a", APERIDC2)), "USUBJID") %>%
#' filter(PARAMCD == "OVRINV") %>%
#' select(USUBJID, ADY, AVALC)
#'
#' ADS <- ASL %>%
#' filter(DISCSTUD == "Y" | !is.na(STDSSDT)) %>%
#' select(USUBJID, STDDRS, STDSDY) %>%
#' rename(ADY = STDSDY, AVALC = STDDRS)
#'
#' # combine with ASL to generate a length for each record
#' ANL <- ASL %>% select(USUBJID, ARMCD, SEX, CADX, TRTDUR) %>%
#' inner_join(rbind(ARS, ADS), "USUBJID")
#'
#' g_swimlane(bar_id = ASL$USUBJID,
#' bar_length = ASL$TRTDUR,
#' sort_by = ASL$ARMCD,
#' col_by = ASL$ARMCD,
#' marker_id = ANL$USUBJID,
#' marker_pos = ANL$ADY,
#' marker_shape = ANL$AVALC,
#' marker_shape_opt <- c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15,
#' "DEATH" = 8, "LOST TO FOLLOW-UP" = 10, "WITHDRAWAL BY SUBJECT" = 14),
#' marker_color = ANL$AVALC,
#' marker_color_opt <- c("CR" = "green", "PR" = "blue", "SD" = "yellow", "PD" = "red",
#' "DEATH" = "black", "LOST TO FOLLOW-UP" = "purple", "WITHDRAWAL BY SUBJECT" = "darkred"),
#' anno_txt = anno_txt,
#' yref_line = c(100, 200),
#' ytick_at = waiver(),
#' # xlab = "Patient ID",
#' ylab = "Time from First Treatment (Day)",
#' title = "Swimlane Plot")
#'
#' }
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
                       # xlab,
                       ylab,
                       title
) {

  # check data
  if (!is.null(sort_by)) tern:::check_same_N(bar_id = bar_id, bar_length = bar_length, sort_by = sort_by)
  if (!is.null(col_by)) tern:::check_same_N(bar_id = bar_id, bar_length = bar_length, col_by = col_by)

  if (!is.null(marker_id) & length(which(!marker_id %in% bar_id)) > 0) stop("marker_id ", marker_id[which(!marker_id %in% bar_id)], " is not in bar_id")

  if (!is.null(marker_id) & !is.null(marker_pos)) tern:::check_same_N(marker_id = marker_id, marker_pos = marker_pos)
  if (!is.null(marker_id) & !is.null(marker_shape)) tern:::check_same_N(marker_id = marker_id, marker_shape = marker_shape)
  if (!is.null(marker_id) & !is.null(marker_color)) tern:::check_same_N(marker_id = marker_id, marker_color = marker_color)

  # data for plot
  bar_data <- data.frame(
    bar_id,
    bar_length,
    sort_by = if (is.null(sort_by)) "x" else tern:::to_n(sort_by, length(bar_length)),
    col_by = if (is.null(col_by)) "x" else tern:::to_n(col_by, length(bar_length))
    )

  #data for marker
  if (is.null(marker_id)) marker_id <- bar_id
  marker_data <- data.frame(
    marker_id,
    marker_pos = if (is.null(marker_pos)) "x" else tern:::to_n(marker_pos, length(marker_id)),
    marker_shape = if (is.null(marker_shape)) "x" else tern:::to_n(marker_shape, length(marker_id)),
    marker_color = if (is.null(marker_color)) "x" else tern:::to_n(marker_color, length(marker_id))
    )

  # if sort by a variable, reorder bar_id; otherwise sort by bar length
  if (!is.null(sort_by)) {
    bar_data$bar_id = factor(bar_data$bar_id, levels = rev(unique(bar_data$bar_id[order(bar_data$sort_by)])))
  } else{
    bar_data$bar_id = factor(bar_data$bar_id, levels = rev(unique(bar_data$bar_id[order(bar_data$bar_length, decreasing = TRUE)])))
  }

  # labeling
  # xlabel <- deparse(substitute(bar_id))
  ylabel <- deparse(substitute(bar_length))
  # xlab <- if (is.null(xlab)) xlabel else xlab
  ylab <- if (is.null(ylab)) ylabel else ylab

  # plot bar plot first
  p <- ggplot(data = bar_data, aes(x = bar_id, y = bar_length)) +
    geom_bar(stat = "identity", aes(fill = col_by)) +
    coord_flip(xlim = c(1,length(unique(bar_id)) + 1)) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    ) +
    # xlab(xlab) +
    ylab(ylab)

  if (!is.null(col_by)) {
    p <- p + guides(fill = guide_legend("Bar Color", order = 1)) +
      theme(
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.key = element_rect(fill = NA))
  }


  # plot marker
  if (!is.null(marker_pos)) {
    p <- p + geom_point(data = marker_data,
                        aes(x = marker_id, y = marker_pos, shape = marker_shape, color = marker_color), na.rm = T) +
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
                    sort_by = if (is.null(sort_by)) "x" else tern:::to_n(sort_by, length(bar_length)))
  } else {
    t <- data.frame(bar_id, bar_length,
                    sort_by = if (is.null(sort_by)) "x" else tern:::to_n(sort_by, length(bar_length)),
                    anno_txt)
  }

  # if sort by a variable, reorder bar_id; otherwise sort by bar length
  if (!is.null(sort_by)) {
    t <- t[with(t, order(sort_by, bar_id)), -c(2,3)]
  } else{
    t <- t[with(t, order(bar_length, bar_id)), -c(2,3)]
  }

  colnames(t)[1] <- " "

  my_theme <- ttheme_default(
    core = list(bg_params = list(fill = NA, col = NA),
                fg_params = list(cex = 0.5)),
    colhead = list(bg_params = list(fill = NA, col = NA),
                   fg_params = list(cex = 0.5)))
  tb <- tableGrob(t, rows = NULL, theme = my_theme)
  tb$heights <- unit(rep(1/nrow(tb), nrow(tb)), "null")

  # grab plot and table as one plot
  g <- ggplotGrob(p)
  g <- gtable_add_cols(g, sum(tb$widths), 0)
  g <- gtable_add_grob(g, tb, t = 6, l = 1)

  grid.newpage()
  grid.draw(g)

}

