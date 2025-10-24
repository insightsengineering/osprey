#' `Swimlane` Plot
#'
#' `Swimlane` plot is often used in Early Development (ED) and displays individual
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
#' @param xref_line numeric vector to plot reference lines
#' @param xtick_at optional break interval of bar length axis
#' @param xlab label for bar length
#' @param title string to be displayed as plot title
#'
#' @template author_qit3
#' @return plot object
#'
#' @export
#'
#' @examplesIf require("nestcolor")
#' # Example 1
#' library(dplyr)
#' library(nestcolor)
#'
#' ADSL <- osprey::rADSL[1:20, ]
#' ADRS <- filter(rADRS, PARAMCD == "OVRINV")
#' ANL <- left_join(ADSL, ADRS, by = c("STUDYID", "USUBJID"), multiple = "all")
#' anno_txt <- ADSL[, c("ARMCD", "SEX")]
#'
#' g_swimlane(
#'   bar_id = ADSL$USUBJID,
#'   bar_length = as.integer(ADSL$TRTEDTM - ADSL$TRTSDTM),
#'   sort_by = ADSL$ARM,
#'   col_by = ADSL$ARM,
#'   marker_id = ANL$USUBJID,
#'   marker_pos = ANL$ADY,
#'   marker_shape = ANL$AVALC,
#'   marker_shape_opt = c("CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 4),
#'   marker_color = NULL,
#'   marker_color_opt = NULL,
#'   anno_txt = anno_txt,
#'   xref_line = c(50, 100),
#'   xtick_at = waiver(),
#'   xlab = "Time from First Treatment (Day)",
#'   title = "Swimlane Plot"
#' )
#'
#' # Example 2
#' library(dplyr)
#' library(nestcolor)
#'
#' ADSL <- osprey::rADSL[1:20, ]
#' ADRS <- osprey::rADRS
#'
#' anno_txt_vars <- c("ARMCD", "SEX", "COUNTRY")
#' anno_txt <- ADSL[, anno_txt_vars]
#'
#' # markers from ADRS
#' ADRS <- dplyr::filter(ADRS, PARAMCD == "OVRINV") %>% select(USUBJID, ADY, AVALC)
#'
#' # markers from ADSL - discontinuation
#' ADS <- ADSL %>%
#'   dplyr::filter(EOSSTT == "Discontinued" | DCSREAS != "") %>%
#'   select(USUBJID, EOSDY, DCSREAS) %>%
#'   dplyr::rename(ADY = EOSDY, AVALC = DCSREAS)
#'
#' # combine ADRS with ADS records as one data for markers and join with ADSL
#' ANL <- inner_join(ADSL, rbind(ADRS, ADS), by = "USUBJID", multiple = "all")
#'
#' g_swimlane(
#'   bar_id = sub(".*-", "", ADSL$USUBJID),
#'   bar_length = as.integer(ADSL$TRTEDTM - ADSL$TRTSDTM),
#'   sort_by = NULL,
#'   col_by = ADSL$ARMCD,
#'   marker_id = sub(".*-", "", ANL$USUBJID),
#'   marker_pos = ANL$ADY,
#'   marker_shape = ANL$AVALC,
#'   marker_shape_opt = c(
#'     "CR" = 16, "PR" = 17, "SD" = 18, "PD" = 15, "NE" = 0,
#'     "Adverse Event" = 7, "Death" = 8, "Physician Decision" = 9, "Progressive Disease" = 10,
#'     "Symptomatic Deterioation" = 11, "Withdrawal by Subject" = 12
#'   ),
#'   marker_color = ANL$AVALC,
#'   marker_color_opt = c(
#'     "CR" = "green", "PR" = "blue", "SD" = "yellow", "PD" = "red",
#'     "NE" = "grey", "Adverse Event" = "orange", "Death" = "black", "Physician Decision" = "navy",
#'     "Progressive Disease" = "purple", "Symptomatic Deterioation" = "cyan",
#'     "Withdrawal by Subject" = "darkred"
#'   ),
#'   anno_txt = anno_txt,
#'   xref_line = c(50, 100),
#'   xtick_at = waiver(),
#'   xlab = "Time from First Treatment (Day)",
#'   title = "Swimlane Plot"
#' )
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
                       xref_line = NULL,
                       xtick_at = waiver(),
                       xlab,
                       title) {
  # check data
  if (!is.null(sort_by)) {
    check_same_N(bar_id = bar_id, bar_length = bar_length, sort_by = sort_by)
  }
  if (!is.null(col_by)) {
    check_same_N(bar_id = bar_id, bar_length = bar_length, col_by = col_by)
  }

  if (!is.null(marker_id) && length(which(!marker_id %in% bar_id)) > 0) {
    stop("marker_id ", marker_id[which(!marker_id %in% bar_id)], " is not in bar_id")
  }

  if (!is.null(marker_id) && !is.null(marker_pos)) {
    check_same_N(marker_id = marker_id, marker_pos = marker_pos)
  }
  if (!is.null(marker_id) && !is.null(marker_shape)) {
    check_same_N(marker_id = marker_id, marker_shape = marker_shape)
  }
  if (!is.null(marker_id) && !is.null(marker_color)) {
    check_same_N(marker_id = marker_id, marker_color = marker_color)
  }
  if (!is.null(xref_line) && (!is.numeric(xref_line) || length(xref_line) == 0)) {
    stop("xref_line must be a non-empty numeric vector or NULL")
  }

  # data for plot
  bar_data <- data.frame(
    bar_id,
    bar_length,
    sort_by = if (is.null(sort_by)) "x" else to_n(sort_by, length(bar_length)),
    col_by = if (is.null(col_by)) "x" else to_n(col_by, length(bar_length))
  )

  # data for marker
  if (is.null(marker_id)) marker_id <- bar_id
  marker_data <- data.frame(
    marker_id,
    marker_pos = if (is.null(marker_pos)) "x" else to_n(marker_pos, length(marker_id)),
    marker_shape = if (is.null(marker_shape)) "x" else to_n(marker_shape, length(marker_id)),
    marker_color = if (is.null(marker_color)) "x" else to_n(marker_color, length(marker_id))
  )

  # if sort by a variable, reorder bar_id by sort var and then bar length; otherwise sort by bar length
  if (!is.null(sort_by)) {
    bar_data$bar_id <- factor(bar_data$bar_id,
      levels = rev(unique(bar_data$bar_id[order(bar_data$sort_by, -bar_data$bar_length)]))
    )
  } else {
    bar_data$bar_id <- factor(bar_data$bar_id,
      levels = rev(unique(bar_data$bar_id[order(-bar_data$bar_length)]))
    )
  }

  # labeling
  xlabel <- deparse(substitute(bar_length))
  xlab <- if (is.null(xlab)) xlabel else xlab

  # plot bar plot first
  p <- ggplot(data = bar_data, aes(x = bar_id, y = bar_length)) +
    geom_bar(stat = "identity", aes(fill = col_by)) +
    coord_flip(xlim = c(1, length(unique(bar_id)) + 1)) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    ) +
    # Note ylab as we have coord_flip above
    ylab(xlab)

  if (is.null(col_by)) {
    p <- p + guides(fill = "none")
  } else {
    p <- p +
      guides(fill = guide_legend("Bar Color", order = 1, ncol = 1)) +
      theme(
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key = element_rect(fill = NA),
        legend.key.width = grid::unit(1, "line"),
        legend.spacing.y = grid::unit(0, "cm"),
        legend.key.height = grid::unit(1, "line")
      )
  }

  limits_x <- NULL

  # plot marker
  if (!is.null(marker_pos)) {
    p <- p + geom_point(
      data = marker_data,
      aes(x = marker_id, y = marker_pos, shape = marker_shape, color = marker_color),
      size = 2.5, na.rm = TRUE
    )
    limits_x <- c(0, max(bar_length, marker_pos, na.rm = TRUE) + 5)

    if (!is.null(marker_shape)) {
      p <- p + guides(shape = guide_legend("Marker Shape", order = 2))
    } else {
      p <- p + guides(shape = "none")
    }

    if (!is.null(marker_color)) {
      p <- p + guides(color = guide_legend("Marker Color", order = 3))
    } else {
      p <- p + guides(color = "none")
    }

    p <- p +
      scale_shape_manual(
        name = "Marker Shape",
        breaks = levels(factor(marker_data$marker_shape)),
        values = if (!is.null(marker_shape_opt)) marker_shape_opt else c(15:25, 0:14)
      )

    if (is.null(marker_color_opt)) {
      if (!is.null(getOption("ggplot2.discrete.colour"))) {
        marker_color_opt <- getOption("ggplot2.discrete.colour")[-seq_len(length(unique(col_by)))]
      } else {
        marker_color_opt <- c("x" = "black")
      }
    }

    p <- p + scale_color_manual(
      name = "Marker Color",
      breaks = levels(factor(marker_data$marker_color)),
      values = marker_color_opt
    )
  }

  # plot reference lines
  if (!is.null(xref_line)) {
    x_axis_min <- pmax(ggplot_build(p)$layout$panel_params[[1]]$x.range[1], 0)
    x_axis_max <- pmax(ggplot_build(p)$layout$panel_params[[1]]$x.range[2], 0)
    xref_line_min <- min(xref_line)
    xref_line_max <- max(xref_line)
    min_res <- min(c(limits_x[1], x_axis_min, xref_line_min))
    max_res <- max(c(limits_x[2], x_axis_max, xref_line_max))
    p <- p + geom_hline(yintercept = xref_line, linetype = "dashed", color = "red") +
      scale_y_continuous(
        limits = c(min_res, max_res),
        breaks = xtick_at,
        expand = c(`if`(min_res == xref_line_min, .01, 0), `if`(max_res == xref_line_max, .01, 0))
      )
  } else {
    if (!is.null(limits_x)) {
      p <- p + scale_y_continuous(limits = limits_x, breaks = xtick_at, expand = c(0, 0))
    }
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
      sort_by = if (is.null(sort_by)) "x" else to_n(sort_by, length(bar_length))
    )
  } else {
    t <- data.frame(bar_id, bar_length,
      sort_by = if (is.null(sort_by)) "x" else to_n(sort_by, length(bar_length)),
      anno_txt
    )
  }

  # if sort by a variable, reorder bar_id; otherwise sort by bar length
  if (!is.null(sort_by)) {
    t <- t[with(t, order(sort_by, -bar_length, levels(as.factor(bar_id)))), -c(2, 3)]
  } else {
    t <- t[with(t, order(-bar_length, levels(as.factor(bar_id)))), -c(2, 3)]
  }

  t <- as.data.frame(t)
  colnames(t)[1] <- " "

  my_theme <- gridExtra::ttheme_default(
    core = list(
      bg_params = list(fill = NA, col = NA),
      fg_params = list(cex = 0.8)
    ),
    colhead = list(
      bg_params = list(fill = NA, col = NA),
      fg_params = list(cex = 0.8)
    )
  )
  tb <- gridExtra::tableGrob(t, rows = NULL, theme = my_theme)
  tb$heights <- grid::unit(rep(1 / nrow(tb), nrow(tb)), "null")

  # grab plot and table as one plot
  g0 <- ggplotGrob(p)
  g1 <- gtable::gtable_add_cols(g0, sum(tb$widths), 0)
  g <- gtable::gtable_add_grob(g1, tb, t = g1$layout[g1$layout$name == "panel", 1], l = 1)

  grid::grid.newpage()
  grid::grid.draw(g)
  invisible(g)
}
