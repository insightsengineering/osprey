
#' Spider Plot
#'
#' Spider plot is often used in Early Development (ED) and displays individual
#' patient plot of an endpoint over time by group.
#'
#'
#' @param marker_x vector of x values (must be in sorted order)
#' @param marker_id vector to group the points together (default
#' should be USUBJID)
#' @param marker_y vector of y values
#' @param line_colby vector defines by what variable plot is color coded,
#' default here is \code{NULL}
#' @param line_color_opt vector defines line color, default here is \code{NULL}
#' @param marker_size size of markers in plot, default here is \code{NULL}
#' @param marker_shape vector defines by what variable points are shape coded,
#' , default here is \code{NULL}
#' @param marker_shape_opt vector defines marker shape code, default here is \code{NULL}
#' @param datalabel_txt list defines text (at last time point) and
#' flag for an arrow annotation
#' (per defined variable) - elements must be labeled \code{txt_ann}/\code{mrkr_all}/\code{mrkr_ann}.
#' \code{txt_ann} - text annotation next to final data point (for text annotation)
#' \code{mrkr_all} - vector of ID's (for annotation marker)
#' \code{mrkr_ann} - vector of ID's (subset of \code{mrkr_all}) where arrow is desired to
#' indicate any study interim points. Default here is \code{NULL}
#' @param facet_rows dataframe defines what variable is used to split the
#' plot into rows, default here is \code{NULL}
#' @param facet_columns dataframe defines what variable is used to split the
#' plot into columns, default here is \code{NULL}
#' @param vref_line value defines vertical line overlay
#' (can be a vector), default here is \code{NULL}
#' @param href_line value defines horizontal line overlay
#' (can be a vector), default here is NULL
#' @param x_label string of text for x axis label, default is time
#' @param y_label string of text for y axis label, default is \% change
#' @param show_legend boolean of whether marker legend is included,
#' default here is \code{FALSE}
#'
#' @return ggplot object
#'
#' @details there is no equivalent STREAM output
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @template author_zhanc107
#'
#' @examples
#' # simple example
#' library(dplyr)
#'
#' ADTR <- rADTR %>% select(STUDYID, USUBJID, ADY, AVISIT, CHG, PCHG, PARAMCD)
#' ADSL <- rADSL %>% select(STUDYID, USUBJID, RACE, SEX, ARM)
#' ANL <- left_join(ADTR, ADSL, by = c("STUDYID", "USUBJID"))
#' ANL <- ANL %>%
#'   dplyr::filter(PARAMCD == "SLDINV" & AVISIT != "POST-BASELINE MINIMUM") %>%
#'   dplyr::filter(RACE %in% c("WHITE", "ASIAN")) %>%
#'   group_by(USUBJID) %>%
#'   dplyr::arrange(ADY) %>%
#'   dplyr::mutate(
#'     CHG = ifelse(AVISIT == "Screening", 0, CHG),
#'     PCHG = ifelse(AVISIT == "Screening", 0, PCHG)
#'   )
#' ANL$USUBJID <- substr(ANL$USUBJID, 14, 18)
#'
#' # Plot 1 - default color and shape mapping
#' g_spiderplot(
#'   marker_x = ANL$ADY,
#'   marker_id = ANL$USUBJID,
#'   marker_y = ANL$PCHG,
#'   line_colby = ANL$USUBJID,
#'   marker_shape = ANL$USUBJID,
#'   # marker_size = 5,
#'   datalabel_txt = list(txt_ann = ANL$USUBJID),
#'   # facet_rows = data.frame(sex = ANL$SEX),
#'   # facet_columns = data.frame(arm = ANL$ARM),
#'   vref_line = c(42, 86),
#'   href_line = c(-20, 20),
#'   x_label = "Time (Days)",
#'   y_label = "Change (%) from Baseline",
#'   show_legend = TRUE
#' )
#'
#' # Plot 2 - with line color mapping
#' g_spiderplot(
#'   marker_x = ANL$AVISIT,
#'   marker_id = ANL$USUBJID,
#'   marker_y = ANL$CHG,
#'   line_colby = ANL$RACE,
#'   line_color_opt = c("WHITE" = "red", "ASIAN" = "blue"),
#'   marker_shape = ANL$USUBJID,
#'   x_label = "Visit",
#'   y_label = "Change from Baseline",
#'   show_legend = TRUE
#' )
g_spiderplot <- function(marker_x,
                         marker_id,
                         marker_y,
                         line_colby = NULL,
                         line_color_opt = NULL,
                         marker_shape = NULL,
                         marker_shape_opt = NULL,
                         marker_size = 3,
                         datalabel_txt = NULL, # USUBJID default
                         facet_rows = NULL,
                         facet_columns = NULL,
                         vref_line = NULL,
                         href_line = NULL,
                         x_label = "Time (Days)",
                         y_label = "Change (%) from Baseline",
                         show_legend = FALSE) {
  check_input_length <- c(nrow(data.frame(marker_x)), nrow(data.frame(marker_id)), nrow(data.frame(marker_y)))

  if (length(unique(check_input_length)) > 1) {
    stop("invalid arguments: check that the length of input arguments are identical")
  }
  if (any(check_input_length == 0)) {
    stop("invalid arguments: check that inputs are not null")
  }

  # set up data-------
  dat <- data.frame(x = marker_x, y = marker_y, group = marker_id)

  if (!is.null(marker_shape)) {
    if (length(unique(c(nrow(data.frame(marker_shape)), check_input_length))) != 1) {
      stop("invalid arguments: check that the length of input arguments are identical")
    }
    dat$sh <- marker_shape
  }
  if (!is.null(facet_rows)) {
    if (length(unique(c(nrow(facet_rows), check_input_length))) != 1) {
      stop("invalid arguments: check that the length of input arguments are identical")
    }
    dat$f_rows <- interaction(facet_rows)
  }
  if (!is.null(facet_columns)) {
    if (length(unique(c(nrow(facet_columns), check_input_length))) != 1) {
      stop("invalid arguments: check that the length of input arguments are identical")
    }
    dat$f_columns <- interaction(facet_columns)
  }
  if (!is.null(line_colby)) {
    if (length(unique(c(nrow(data.frame(line_colby)), check_input_length))) != 1) {
      stop("invalid arguments: check that the length of input arguments are identical")
    }
    dat$l_col <- line_colby
  }
  if (!is.null(datalabel_txt$txt_ann)) {
    dat$lbl_all <- datalabel_txt$txt_ann

    dat <- dat %>%
      group_by(.data$lbl_all) %>%
      dplyr::mutate(lab = ifelse(.data$x == last(.data$x), as.character(.data$lbl_all), " "))
  }
  if (!is.null(datalabel_txt$mrkr_all) && !is.null(datalabel_txt$mrkr_ann)) {
    if (length(unique(c(nrow(datalabel_txt$mrkr_all), check_input_length))) != 1) {
      stop("invalid arguments: check that the length of input arguments are identical")
    }
    dat$id <- datalabel_txt$mrkr_all
  }

  dat <- dat %>% as.data.frame()

  # plot spider plot----------------- this section can be condensed later
  pl <- ggplot(data = dat, aes(x = .data$x, y = .data$y, group = .data$group)) +
    xlab(x_label) +
    ylab(y_label) +
    theme(legend.position = "top", legend.title = element_blank())

  pl <- pl + geom_hline(yintercept = 0, linetype = "solid", color = "gray", size = 1)


  pl <- pl +
    geom_line(mapping = if (!is.null(line_colby)) {
        aes(color = .data$l_col)
      } else {
        NULL
      },
      size = 1,
      alpha = 0.5,
      show.legend = show_legend)

  # marker shape------------ this section can be condensed later
  if (!is.null(marker_shape)) {
      pl <- pl +
        geom_point(mapping = if (!is.null(line_colby)) {
            aes(shape = .data$sh, color = .data$l_col)
          } else {
            aes(shape = .data$sh)
          }, size = marker_size, show.legend = show_legend)

  } else if (is.null(marker_shape)) {
      pl <- pl +
        geom_point(mapping = if (!is.null(line_colby)) {
            aes(color = .data$l_col)
          } else {
            NULL
          }, size = 3, show.legend = show_legend)
    }

  # label at last data point---------
  if (!is.null(datalabel_txt)) {
    if (!is.null(datalabel_txt$txt_ann) && is.null(datalabel_txt$mrkr_all) && is.null(datalabel_txt$mrkr_ann)) {
      pl <- pl +
        geom_text(data = dat,
                  aes(x = .data$x, y = .data$y, label = .data$lab), hjust = -0.3,
                  size = 4,
                  show.legend = FALSE)
    } else if (is.null(datalabel_txt$txt_ann) &&
               !is.null(datalabel_txt$mrkr_all) &&
               !is.null(datalabel_txt$mrkr_ann)) {
      dat_arrow <- dat %>%
        dplyr::filter(id %in% datalabel_txt$mrkr_ann) %>%
        group_by(.data$id) %>%
        dplyr::filter(.data$x == last(.data$x))
      pl <- pl +
        geom_segment(data = dat_arrow,
                     mapping = aes(x = .data$x, y = .data$y, xend = .data$x, yend = .data$y),
                     arrow = arrow(length = unit(0.15, "inches"), ends = "first", type = "closed"),
                     size = 0.4,
                     color = "black",
                     show.legend = FALSE)
    } else if (!is.null(datalabel_txt$txt_ann) &&
               !is.null(datalabel_txt$mrkr_all) &&
               !is.null(datalabel_txt$mrkr_ann)) {
      pl <- pl +
        geom_text(data = dat,
                  aes(x = .data$x, y = .data$y, label = .data$lab),
                  hjust = -0.45,
                  size = 4,
                  show.legend = FALSE)

      dat_arrow <- dat %>%
        dplyr::filter(id %in% datalabel_txt$mrkr_ann) %>%
        group_by(.data$id) %>%
        dplyr::filter(.data$x == last(.data$x))

      pl <- pl +
        geom_segment(data = dat_arrow,
                     mapping = aes(x = .data$x, y = .data$y, xend = .data$x, yend = .data$y),
                     arrow = arrow(length = unit(0.15, "inches"), ends = "first", type = "closed"),
                     size = 0.4,
                     color = "black",
                     show.legend = FALSE)
    }
  }

  # vertical and horizontal reference lines
  if (!is.null(href_line)) {
    pl <- pl + geom_hline(yintercept = href_line, linetype = "dotted", color = "black")
  }

  if (!is.null(vref_line)) {
    for (i in 1:length(vref_line)) {
      pl <- pl +
        annotate("segment",
                 x = vref_line[i],
                 y = -Inf,
                 xend = vref_line[i],
                 yend = Inf,
                 linetype = "dotted",
                 color = "black")
    }
  }

  # facets---------------
  if (is.null(facet_rows) && is.null(facet_columns)) {
    pl
  } else if (is.null(facet_rows) && !is.null(facet_columns)) {
    pl <- pl + facet_grid(. ~ f_columns)
  } else if (is.null(facet_columns) && !is.null(facet_rows)) {
    pl <- pl + facet_grid(f_rows ~ .)
  } else {
    pl <- pl + facet_grid(f_rows ~ f_columns)
  }

  # simple function to call a vector of color values
  call_color <- function(len) {
    dat_col <- data.frame(color_opt = colors())
    dat_col <- dat_col %>%
      dplyr::filter(!grepl("white", .data$color_opt)) %>%
      droplevels()

    return(dat_col[1:len, 1])
  }

  # remove marker from color legend
  if (!is.null(line_colby)) {
    pl <- pl + guides(color = guide_legend(override.aes = list(shape = rep(NA, length(unique(dat$l_col))))))

    if (!is.null(line_color_opt)) {
      pl <- pl + scale_color_manual(
        name = "Color",
        breaks = dat$l_col,
        values = line_color_opt
      )
    }
  }

  if (!is.null(marker_shape) && is.null(marker_shape_opt)) {
    symbol_val <- c(15:18, 1:14)
    len <- length(unique(dat$sh))
    symbol_val <- rep(symbol_val, ceiling(len / 26))

    pl <- pl + scale_shape_manual(
      name = "Shape",
      breaks = sort(dat$sh),
      values = symbol_val[1:len],
      guide = guide_legend(override.aes = list(linetype = rep("blank", len)))
    )
  }

  if (!is.null(marker_shape_opt)) {
    pl <- pl + scale_shape_manual(
      name = "Shape",
      breaks = dat$sh,
      values = marker_shape_opt
    )
  }

  # modify background color
  pl <- pl + annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf) +
    theme_bw() +
    theme(
      strip.background = element_rect(linetype = "blank", fill = "white"),
      text = element_text(size = 16),
      axis.text = element_text(color = "black"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    ) +
    labs(shape = "Shape", color = "Color") # +

  if (is.numeric(marker_x)) {
    pl <- pl + xlim(min(marker_x), max(marker_x) * 1.3)
  } else {
    pl <- pl +
      scale_x_discrete(expand = c(0.3, 0)) +
      theme(axis.text.x = element_text(angle = 90))
  }

  grid.draw(pl)
  invisible(pl)
}
