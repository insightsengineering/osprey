#' Swimlane Plot
#' Swimlane bar plot for each id with markers
#'
#' @param bar_by variable to identify each bar. Default is \code{ID}.
#' @param bar_length length variable for each bar
#' @param sort_by variable to sort bars
#' @param col_by variable to color bars
#' @param marker_by variable to order markers
#' @param marker_shape variable to specify shape and color for markers
#' @param marker_pos variable to specify position for markers
#' @param anno_txt description in the left table
#' @param ytick_at break interval of y-axis. It takes a numeric vector or \code{NULL}
#' @param yref_line numeric vector to plot reference lines
#' @param xlab x label
#' @param ylab y label
#' @param title title
#'
#' @author qit3
#'
#' @import ggplot2 grid gridExtra gtable
#'
#' @return plot object
#'
#' @export g_swimlane
#'
#' @examples
#'
#' library(random.cdisc.data)
#' ASL <- radam("ASL", N=50, start_with = list(TRTDUR = rexp(50, 1/100)))
#' ARS <- radam("ARS", ADSL = ASL,
#' start_with = list(
#'       ADY = rexp(nrow(ASL), 1/80)
#'    )) %>% filter(PARAMCD == "OVRINV")
#' ANL <- ASL %>% left_join(ARS, by = c("STUDYID", "USUBJID")) %>%
#' # pre-process bar length breaks for treatment duration for longitudinal records
#' group_by(USUBJID) %>%
#' mutate(n = n(), bar_len = TRTDUR/n)
#' anno_txt <- ANL[, c("ARMCD", "SEX", "RACE")]
#'
#' g_swimlane(bar_by = ANL$USUBJID,
#' bar_length = ANL$bar_len ,
#' sort_by = ANL$ARM ,
#' col_by = ANL$ARM ,
#' marker_by = ANL$STUDYID,
#' marker_shape = ANL$AVALC,
#' marker_pos = ANL$ADY ,
#' anno_txt = anno_txt,
#' yref_line = c(100, 200),
#' ytick_at = waiver(),
#' xlab = "Patient ID",
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
#' ARS <- ASL %>% select(USUBJID) %>%
#' left_join(xars %>% filter(grepl("1a", APERIDC2)), "USUBJID") %>%
#' filter(PARAMCD == "OVRINV") %>%
#' select(USUBJID, ADY, AVALC) %>%
#' mutate(domain = "Response")
#'
#' ADS <- ASL %>%
#' filter(DISCSTUD == "Y" | !is.na(STDSSDT)) %>%
#' select(USUBJID, STDDRS, STDSDY) %>%
#' rename(ADY = STDSDY, AVALC = STDDRS) %>%
#' mutate(domain = "Discon")
#'
#' # combine with ASL to generate a length for each record
#' ANL <- ASL %>% select(USUBJID, ARMCD, SEX, CADX, TRTDUR) %>%
#' inner_join(rbind(ARS, ADS), "USUBJID") %>%
#' # pre-process bar length breaks for treatment duration for longitudinal records
#' group_by(USUBJID) %>%
#' mutate(n = n(), bar_len = TRTDUR/n)
#' anno_txt_var <- c("ARMCD", "SEX", "CADX")
#' anno_txt <- ANL[, anno_txt_var]
#'
#' g_swimlane(bar_by = ANL$USUBJID,
#' bar_length = ANL$bar_len ,
#' sort_by = ANL$ARMCD ,
#' col_by = ANL$ARMCD ,
#' marker_by = ANL$domain,
#' marker_shape = ANL$AVALC,
#' marker_pos = ANL$ADY ,
#' anno_txt = NULL,
#' yref_line = c(100, 200, 300),
#' ytick_at = waiver(),
#' xlab = "Patient ID",
#' ylab = "Time from First Treatment (Day)",
#' title = "Swimlane Plot")
#'
#' }
#'

g_swimlane <- function(bar_by,
                       bar_length,
                       sort_by,
                       col_by,
                       marker_by,
                       marker_shape,
                       marker_pos,
                       anno_txt,
                       ytick_at,
                       yref_line,
                       xlab,
                       ylab,
                       title
) {

  # data for plot
  plotdata <- data.frame(bar_by, bar_length, sort_by, col_by, marker_by, marker_shape, marker_pos)

  # if sort by a variable, reorder bar_by
  plotdata$bar_by = factor(plotdata$bar_by, levels = rev(unique(plotdata$bar_by[order(sort_by)])))

  # plot swimlane
  p <- ggplot(data = plotdata, aes(x = bar_by, y = bar_length, group = bar_by, fill = col_by)) +
    geom_bar(stat = "identity") +
    geom_point(aes(x = bar_by, y = marker_pos, shape = marker_shape, color = marker_shape), na.rm = T) +
    geom_hline(yintercept = yref_line, linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(0, max(marker_pos) + 5), breaks = ytick_at, expand = c(0, 0)) +
    labs(y = ylab, x = xlab, title = title, fill = "") +
    coord_flip(xlim = c(1,length(unique(bar_by)) + 1)) +
    scale_colour_manual(name = "Events", guide = "colorbar",
                        breaks = rev(unique(marker_shape[order(marker_by, marker_shape)])),
                        values = c(2, 3, 4, 5, 6)) +
    scale_shape_manual(name = "Events",
                       breaks = rev(unique(marker_shape[order(marker_by, marker_shape)])),
                       values = c(15, 16, 17, 18, 19, 20)) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    # guides(
    #   color = guide_legend(order = 3),
    #   fill = guide_legend(override.aes = list(shape = NA), order = 1)
    # ) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7)
    )

  # create annotation as a separate plot
  if (is.null(anno_txt)) {
    t <- data.frame(bar_by, sort_by)
  } else {
    t <- data.frame(bar_by, sort_by, anno_txt)
  }

  t <- t[!duplicated(t),]
  t <- t[with(t, order(sort_by, bar_by)), -2]
  t <- as.data.frame(t)
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

