#' Waterfall Plot
#'
#' Waterfall plot is often used in Early Development (ED) to present each individual patient’s best
#' response to a particular drug based on a parameter.
#'
#' @param bar_id (`vector`)\cr contains IDs to identify each bar
#' @param bar_height numeric vector to be plotted as height of each bar
#' @param sort_by (`vector`)\cr used to sort bars, default is `NULL` in which case bars are ordered
#'   by decreasing height
#' @param col_by (`vector`)\cr used to color bars, default is `NULL` in which case bar_id is taken if
#'   the argument \code{bar_color_opt} is provided
#' @param bar_color_opt (`vector`)\cr
#'   aesthetic values to map color values (named vector to map color values to each name).
#'   If not `NULL`, please make sure this contains all possible values for \code{col_by} values,
#'   otherwise default ggplot color will be assigned, please note that `NULL` needs to be specified
#'   in this case
#' @param anno_txt (`dataframe`)\cr
#'   contains subject-level variables to be displayed as annotation below the waterfall plot,
#'   default is `NULL`
#' @param href_line (`numeric vector`)\cr to plot horizontal reference lines, default is `NULL`
#' @param facet_by (`vector`)\cr to facet plot and annotation table, default is `NULL`
#' @param show_datavalue (`boolean`)\cr controls whether value of bar height is shown, default is \code{TRUE}
#' @param add_label (`vector`)\cr of one subject-level variable to be added to each bar except for bar_height,
#'   default is `NULL`
#' @param gap_point (`numeric`)\cr value for adding bar break when some bars are significantly higher than
#'   others, default is `NULL`
#' @param ytick_at (`numeric`)\cr optional bar height axis interval, default is 20
#' @param y_label (`string`)\cr label for bar height axis, default is "Best % Change from Baseline"
#' @param title (`string`)\cr displayed as plot title, default is "Waterfall Plot"
#'
#' @author Xuefeng Hou (houx14) \email{houx14@gene.com}
#' @template author_qit3
#'
#' @return plot object
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' library(dplyr)
#' library(nestcolor)
#'
#' g_waterfall(
#'   bar_id = letters[1:3], bar_height = c(3, 5, -1),
#'   bar_color_opt = c("red", "green", "blue")
#' )
#'
#' # Example 1
#' ADSL <- rADSL[1:15, ]
#' ADRS <- rADRS %>%
#'   filter(USUBJID %in% ADSL$USUBJID)
#' ADTR <- rADTR %>%
#'   filter(USUBJID %in% ADSL$USUBJID) %>%
#'   select(USUBJID, PCHG) %>%
#'   group_by(USUBJID) %>%
#'   slice(which.min(PCHG))
#'
#' TR_SL <- ADSL %>%
#'   inner_join(ADTR, "USUBJID")
#'
#' SUB_ADRS <- ADRS %>%
#'   filter(PARAMCD == "BESRSPI" | PARAMCD == "OBJRSPI") %>%
#'   select(USUBJID, PARAMCD, AVALC, AVISIT, ADY) %>%
#'   spread(PARAMCD, AVALC)
#'
#' ANL <- TR_SL %>%
#'   left_join(SUB_ADRS, "USUBJID")
#'
#' anno_txt_vars <- c("TRTDURD", "BESRSPI", "OBJRSPI", "SEX", "BMK2")
#'
#' g_waterfall(
#'   bar_height = ANL$PCHG,
#'   bar_id = sub(".*-", "", ANL$USUBJID),
#'   col_by = ANL$SEX,
#'   sort_by = ANL$ARM,
#'   # bar_color_opt = c("F" = "red", "M" = "green", "U" = "blue"),
#'   anno_txt = ANL[, anno_txt_vars],
#'   facet_by = NULL,
#'   href_line = c(-30, 20),
#'   add_label = ANL$BESRSPI,
#'   ytick_at = 20,
#'   gap_point = NULL,
#'   show_datavalue = TRUE,
#'   y_label = "Best % Change from Baseline",
#'   title = "Waterfall Plot"
#' )
#'
#' # Example 2 facetting
#' anno_txt_vars <- c("BESRSPI", "OBJRSPI")
#'
#' g_waterfall(
#'   bar_id = sub(".*-", "", ANL$USUBJID),
#'   bar_height = ANL$PCHG,
#'   sort_by = ANL$COUNTRY,
#'   col_by = ANL$SEX,
#'   bar_color_opt = c("F" = "tomato", "M" = "skyblue3", "U" = "darkgreen"),
#'   anno_txt = ANL[, anno_txt_vars],
#'   facet_by = ANL$STRATA2,
#'   href_line = c(-30, 20),
#'   add_label = ANL$BESRSPI,
#'   ytick_at = 20,
#'   gap_point = 260,
#'   y_label = "Best % Change from Baseline",
#'   title = "Waterfall Plot"
#' )
#'
#' # Example 3 extreme value
#' ANL$PCHG[3] <- 99
#' ANL$PCHG[5] <- 199
#' ANL$PCHG[7] <- 599
#' ANL$BESRSPI[3] <- "PD"
#' ANL$BESRSPI[5] <- "PD"
#' ANL$BESRSPI[7] <- "PD"
#'
#' g_waterfall(
#'   bar_id = sub(".*-", "", ANL$USUBJID),
#'   bar_height = ANL$PCHG,
#'   sort_by = ANL$ARM,
#'   col_by = ANL$SEX,
#'   bar_color_opt = c("F" = "tomato", "M" = "skyblue3", "U" = "darkgreen"),
#'   anno_txt = ANL[, anno_txt_vars],
#'   facet_by = NULL,
#'   href_line = c(-30, 20),
#'   add_label = ANL$BESRSPI,
#'   ytick_at = 20,
#'   gap_point = 260,
#'   y_label = "Best % Change from Baseline",
#'   title = "Waterfall Plot"
#' )
g_waterfall <- function(bar_id,
                        bar_height,
                        sort_by = NULL,
                        col_by = NULL,
                        bar_color_opt = NULL,
                        anno_txt = NULL,
                        href_line = NULL,
                        facet_by = NULL,
                        show_datavalue = TRUE,
                        add_label = NULL,
                        gap_point = NULL,
                        ytick_at = 20,
                        y_label = "Best % Change from Baseline",
                        title = "Waterfall Plot") {
  # check data
  check_input_length <- c(nrow(data.frame(bar_id)), nrow(data.frame(bar_height)))

  if (length(unique(check_input_length)) > 1) {
    stop("invalid arguments: check that the length of input arguments are identical")
  }

  if (any(check_input_length == 0)) {
    stop("invalid arguments: check that inputs are not null")
  }

  if (!is.null(bar_color_opt)) {
    if (is.null(col_by)) {
      col_by <- bar_id
    } else {
      ls1 <- levels(factor(col_by))
      ls2 <- names(bar_color_opt)
      if (length(intersect(ls1, ls2)) == 0) {
        stop("invalid argument: check that the col_by and bar_color_opt have overlapping categories")
      } else if (length(ls2) < length(ls1)) {
        stop("invalid argument: More categories in col_by than the ones listed in bar_color_opt")
      }
    }
  }

  if (!is.null(sort_by)) check_same_N(bar_id = bar_id, bar_height = bar_height, sort_by = sort_by)
  if (!is.null(col_by)) check_same_N(bar_id = bar_id, bar_height = bar_height, col_by = col_by)

  facet_plot <- function(bar_id = bar_id,
                         bar_height = bar_height,
                         sort_by = sort_by,
                         col_by = col_by,
                         bar_color_opt = bar_color_opt,
                         anno_txt = anno_txt,
                         href_line = href_line,
                         facet_by = facet_by,
                         show_datavalue = show_datavalue,
                         add_label = add_label,
                         gap_point = gap_point,
                         ytick_at = ytick_at,
                         y_label = y_label,
                         title = title) {
    # Data for plot
    bar_data <- data.frame(
      bar_id,
      bar_height,
      add_label = if (is.null(add_label)) to_n("x", length(bar_height)) else to_n(add_label, length(bar_height)),
      sort_by = if (is.null(sort_by)) to_n("x", length(bar_height)) else to_n(sort_by, length(bar_height)),
      col_by = if (is.null(col_by)) to_n("x", length(bar_height)) else to_n(col_by, length(bar_height))
    )

    # if sort by a variable, reorder bar_id by sort var and then bar length; otherwise sort by bar length
    if (!is.null(sort_by)) {
      bar_data$bar_id <- factor(
        bar_data$bar_id,
        levels = unique(bar_data$bar_id[order(bar_data$sort_by, bar_data$bar_height, decreasing = TRUE)])
      )
    } else {
      bar_data$bar_id <- factor(
        bar_data$bar_id,
        levels = unique(bar_data$bar_id[order(bar_data$bar_height, decreasing = TRUE)])
      )
    }

    # bar_id has already been sorted based on bar_height so keeping the 1st entry is equivalent to keeping
    # the best % change from baseline
    bar_data <- bar_data %>% distinct(bar_id, .keep_all = TRUE)

    # plot bar plot
    if (is.null(gap_point)) {
      ybreaks <- seq(
        ytick_at * floor(min(bar_data$bar_height, na.rm = TRUE) / ytick_at),
        ytick_at * ceiling(max(bar_data$bar_height, na.rm = TRUE) / ytick_at),
        by = ytick_at
      )

      p <- ggplot(data = bar_data, aes(x = bar_id, y = bar_height)) +
        geom_col(position = "identity", aes(fill = col_by)) +
        scale_y_continuous(breaks = ybreaks) +
        scale_x_discrete(expand = expansion(add = 0.5)) +
        geom_hline(yintercept = 0, colour = "black") +
        theme_bw() +
        theme(
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
          axis.title.x = element_blank()
        ) +
        ylab(y_label)
    } else if (all(is.na(bar_height))) {
      ybreaks <- seq(ytick_at * -2, ytick_at * 2, by = ytick_at)

      p <- ggplot(data = bar_data, aes(x = bar_id, y = bar_height)) +
        geom_col(position = "identity", aes(fill = col_by)) +
        scale_y_continuous(breaks = ybreaks) +
        scale_x_discrete(expand = expansion(add = 0.5)) +
        geom_hline(yintercept = 0, colour = "black") +
        theme_bw() +
        theme(
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
          axis.title.x = element_blank()
        ) +
        ylab(y_label)
    } else if (max(bar_height, na.rm = TRUE) <= gap_point) {
      ybreaks <- seq(
        ytick_at * floor(min(bar_data$bar_height, na.rm = TRUE) / ytick_at),
        ytick_at * ceiling(max(bar_data$bar_height, na.rm = TRUE) / ytick_at),
        by = ytick_at
      )

      p <- ggplot(data = bar_data, aes(x = bar_id, y = bar_height)) +
        geom_col(position = "identity", aes(fill = col_by)) +
        scale_y_continuous(breaks = ybreaks) +
        scale_x_discrete(expand = expansion(add = 0.5)) +
        geom_hline(yintercept = 0, colour = "black") +
        theme_bw() +
        theme(
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
          axis.title.x = element_blank()
        ) +
        ylab(y_label)
    } else {
      length_cut <- ytick_at *
        floor(0.8 * (min(bar_data$bar_height[bar_data$bar_height > gap_point], na.rm = TRUE) - gap_point) / ytick_at)

      cut_fun <- function(x) {
        if (is.na(x)) {
          x <- x
        } else if (x > gap_point) {
          x <- x - length_cut
        } else {
          x <- x
        }
      }

      bar_data$new_bar_height <- sapply(bar_data$bar_height, cut_fun)

      ylabel1 <- seq(
        ytick_at * floor(min(bar_data$bar_height, na.rm = TRUE) / ytick_at),
        ytick_at * floor(gap_point / ytick_at),
        by = ytick_at
      )
      ylabel2 <- seq(
        ytick_at * floor((gap_point + length_cut) / ytick_at),
        ytick_at * ceiling(max(bar_data$bar_height, na.rm = TRUE) / ytick_at),
        by = ytick_at
      )

      ybreaks <- seq(
        ytick_at * floor(min(bar_data$new_bar_height, na.rm = TRUE) / ytick_at),
        ytick_at * ceiling(max(bar_data$new_bar_height, na.rm = TRUE) / ytick_at),
        by = ytick_at
      )

      if (length(ylabel1) + length(ylabel2) == length(ybreaks)) {
        ylabels <- c(ylabel1, ylabel2)
      } else {
        dif <- length(ylabel1) + length(ylabel2) - length(ybreaks)
        ylabel2 <- ylabel2[-(1:dif)] # nolint
        ylabels <- c(ylabel1, ylabel2)
      }

      p <- ggplot(data = bar_data, aes_string(x = "bar_id", y = "new_bar_height")) +
        geom_col(position = "identity", aes(fill = col_by)) +
        geom_rect(aes(xmin = 0.5, xmax = length(bar_id), ymin = gap_point, ymax = gap_point + 3), fill = "white") +
        scale_y_continuous(breaks = ybreaks, labels = ylabels) +
        scale_x_discrete(expand = expansion(add = 0.5)) +
        geom_hline(yintercept = 0, colour = "black") +
        theme_bw() +
        theme(
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
          axis.title.x = element_blank()
        ) +
        ylab(y_label)
    }

    if (show_datavalue == TRUE) {
      p <- p + geom_text(
        label = format(bar_data$bar_height, digits = 1),
        size = 2.5,
        vjust = ifelse(bar_data$bar_height >= 0, -0.5, 1.5)
      )
    }

    if (!is.null(add_label)) {
      p <- p + geom_text(
        aes(x = bar_id, y = 0, label = add_label),
        size = 3,
        vjust = ifelse(bar_data$bar_height >= 0, 1.5, -0.5)
      )
    }

    if (is.null(col_by)) {
      p <- p + guides(fill = FALSE)
    } else {
      p <- p + guides(fill = guide_legend("Bar Color", order = 1, ncol = 1)) +
        theme(
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9),
          legend.key = element_rect(fill = NA)
        )
    }

    if (is.null(bar_color_opt) && !is.null(getOption("ggplot2.discrete.colour"))){
      bar_color_opt <- getOption("ggplot2.discrete.colour")
    }
    if (!is.null(bar_color_opt)) {
      p <- p + scale_fill_manual(values = bar_color_opt)
    }

    # plot reference lines
    if (!is.null(href_line)) {
      p <- p + geom_hline(yintercept = href_line, linetype = "dashed", color = "red")
    }

    # add plot title
    if (is.null(facet_by)) {
      p <- p +
        labs(title = title) +
        theme(plot.title = element_text(face = "bold"))
    } else {
      p <- p +
        labs(title = paste(title, "-", as.character(unique(facet_by)))) +
        theme(plot.title = element_text(face = "bold"))
    }

    if (!is.null(anno_txt)) {
      t_anno <- data.frame(
        bar_id,
        bar_height,
        sort_by = if (is.null(sort_by)) to_n("x", length(bar_height)) else to_n(sort_by, length(bar_height)),
        anno_txt
      )

      # if sort by a variable, reorder bar_id; otherwise sort by bar length
      if (!is.null(sort_by)) {
        t_anno$bar_id <- factor(
          t_anno$bar_id,
          levels = unique(t_anno$bar_id[order(t_anno$sort_by, t_anno$bar_height, decreasing = TRUE)])
        )
      } else {
        t_anno$bar_id <- factor(
          t_anno$bar_id,
          levels = unique(t_anno$bar_id[order(t_anno$bar_height, decreasing = TRUE)])
        )
      }

      # bar_id has already been sorted based on bar_height so keeping the 1st entry is equivalent to keeping
      # the best % change from baseline
      t_anno <- t_anno %>% distinct(bar_id, .keep_all = TRUE)
      t_anno <- t_anno[order(t_anno$bar_id), ]
      t_anno <- t_anno[, -c(1, 2, 3)]

      t_anno <- t(t_anno)

      my_theme <- gridExtra::ttheme_default(
        core = list(bg_params = list(fill = NA, col = NA), fg_params = list(cex = 0.8)),
        rowhead = list(bg_params = list(fill = NA, col = NA), fg_params = list(cex = 0.8)),
        padding = grid::unit(c(0, 4), "mm")
      )

      tb <- gridExtra::tableGrob(
        t_anno,
        rows = NULL,
        cols = NULL,
        theme = my_theme
      )

      tb$widths <- grid::unit(rep(1 / (ncol(tb)), ncol(tb)), "null")
      tb <- gtable::gtable_add_grob(
        tb,
        grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
        t = 1, b = nrow(tb), l = 1, r = ncol(tb)
      )

      t_anno_name <- names(anno_txt)
      tb_rowname <- gridExtra::tableGrob(
        t_anno_name,
        rows = NULL,
        cols = NULL,
        theme = gridExtra::ttheme_minimal(
          core = list(bg_params = list(fill = NA, col = NA), fg_params = list(cex = 0.8))
        )
      )

      # grab plot and table as one plot
      g0 <- ggplotGrob(p)
      g1 <- gtable::gtable_add_rows(g0, sum(tb$heights), pos = -1)
      g2 <- gtable::gtable_add_grob(
        g1,
        tb,
        t = -1,
        l = g1$layout[g1$layout$name == "panel", 2],
        r = g1$layout[g1$layout$name == "panel", 4]
      )
      g3 <- gtable::gtable_add_cols(g2, tb_rowname$widths, pos = 0)
      g <- gtable::gtable_add_grob(g3, tb_rowname, t = -1, l = 2)
    } else {
      p <- p +
        theme(axis.title.x = element_text()) +
        labs(x = "Unique Subject ID")
      g <- ggplotGrob(p)
    }
    g
  }

  if (is.null(facet_by)) {
    gt <- facet_plot(
      bar_id = bar_id,
      bar_height = bar_height,
      sort_by = sort_by,
      col_by = col_by,
      bar_color_opt = bar_color_opt,
      anno_txt = anno_txt,
      href_line = href_line,
      facet_by = facet_by,
      show_datavalue = show_datavalue,
      add_label = add_label,
      gap_point = gap_point,
      ytick_at = ytick_at,
      y_label = y_label,
      title = title
    )
  } else {
    facet_by <- factor(facet_by)
    g_list <- rep(list(NA), length(levels(facet_by)))
    for (i in seq_along(levels(facet_by))) {
      facet_level <- levels(facet_by)[i]
      g_list[[i]] <- facet_plot(
        bar_id = bar_id[facet_by == facet_level],
        bar_height = bar_height[facet_by == facet_level],
        sort_by = sort_by[facet_by == facet_level],
        col_by = col_by[facet_by == facet_level],
        bar_color_opt = bar_color_opt,
        anno_txt = anno_txt[facet_by == facet_level, , drop = FALSE],
        href_line = href_line,
        facet_by = facet_by[facet_by == facet_level],
        show_datavalue = show_datavalue,
        add_label = add_label[facet_by == facet_level],
        gap_point = gap_point,
        ytick_at = ytick_at,
        y_label = y_label,
        title = title
      )
    }

    gt <- gridExtra::grid.arrange(grobs = g_list, ncol = 1, nrow = length(levels(facet_by)))
  }

  grid::grid.newpage()
  grid::grid.draw(gt)
  invisible(gt)
}
