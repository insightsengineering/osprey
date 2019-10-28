#' Butterfly Plot
#'
#'
#' The butterfly plot is often used in Early Development (ED) and is an opposed
#' barplot that shows instances of AEs or # of patients by category separated by
#' a dichotomization variable. Each bar can be color coded according
#' to a variable of choice and sorted according to either alphabetical order or the
#' maximum count.
#'
#'
#' @param category vector of y values
#' @param right_flag vector of 1/0 represents right side of barplot
#' @param left_flag vector of 1/0 represents left side of barplot
#' @param group_names string vector of length 2 with desired names of dichotomization variables
#' required format : first name corresponds to the name of the right side
#'                   second name corresponds to name of the left side
#' default: will extract column names from group
#' @param block_count string - what to count by (ex: # of AEs or # of patients)
#' @param block_color vector - color coding of bar segments
#' @param id unique subject identifier variable.
#' @param facet_rows vector defines what variable is used to split the
#' plot into rows, default here is NULL
#' @param x_label string of text for x axis label, default is block_count
#' @param y_label string of text for y axis label, default is AE Derived Terms
#' @param legend_label \code{character} for legend label, default is AETOXGR
#' @param sort_by character string that defines the ordering of the class and term
#' variables in the output table,
#' options: "alphabetical" or "count", default here is set to "count"
#' @param show_legend boolean of whether color coding legend is included,
#' default here is FALSE
#'
#' @details there is no equivalent STREAM output
#'
#' @return ggplot object
#'
#' @importFrom plyr ddply
#' @importFrom stringr str_wrap
#' @importFrom rlang .data
#' @importFrom gtable gtable_add_grob
#' @importFrom grid grid.draw gpar grid.text
#'
#' @export
#'
#' @template author_zhanc107
#' @template author_qit3
#'
#' @examples
#' library(dplyr)
#'
#' ADSL <- rADSL %>% select(USUBJID, STUDYID, SEX, ARM, RACE) %>% dplyr::filter(SEX %in% c("F", "M"))
#' AAE <- rADAE %>% select(USUBJID, STUDYID, AEBODSYS, AETOXGR)
#'
#' ANL <- left_join(AAE, ADSL, by = c("STUDYID", "USUBJID"))
#' ANL <- ANL %>%
#'   dplyr::mutate(r_flag = ifelse(RACE == "ASIAN", 1, 0)) %>%
#'   dplyr::mutate(l_flag = ifelse(SEX == "M", 1, 0))
#' ANL <- na.omit(ANL)
#' ANL <- ANL %>% dplyr::filter(AEBODSYS %in% c(
#'   "Investigations", "Vascular disorders",
#'   "Musculoskeletal and connective tissue disorders"
#' ))
#'
#' \donttest{
#' g_butterfly(
#'   category = ANL$AEBODSYS,
#'   right_flag = ANL$r_flag,
#'   left_flag = ANL$l_flag,
#'   group_names = c("r_flag Asian", "l_flag M"),
#'   block_count = "# of AEs",
#'   block_color = ANL$AETOXGR,
#'   id = ANL$USUBJID,
#'   x_label = "# of patients",
#'   y_label = "AE Derived Terms",
#'   legend_label = "AETOXGR",
#'   sort_by = "count",
#'   show_legend = TRUE
#' )
#' }
g_butterfly <- function(category = NULL,
                        right_flag = NULL,
                        left_flag = NULL,
                        id = NULL,
                        group_names = NULL,
                        block_count = "# of patients",
                        block_color = NULL,
                        facet_rows = NULL,
                        x_label = block_count,
                        y_label = "AE Derived Terms",
                        legend_label = "AETOXGR",
                        sort_by = "alphabetical",
                        show_legend = TRUE) {
  stop_if_not(
    list(!is.empty(category), "missing argument: category must be specified"),
    list(!is.empty(right_flag), "missing argument: right_flag must be specified"),
    list(!is.empty(left_flag), "missing argument: left_flag must be specified"),
    list(!is.empty(id), "missing argument: id must be specified"),

    list(length(unique(vapply(list(category, right_flag, left_flag, id), length, integer(1)))) == 1,
         "invalid arguments: check that the length of input arguments are identical"),

    list(length(unique(right_flag)) == 2 && length(unique(left_flag)),
         "invalid arguments: groups can only have 2 unique values"),

    list(is.null(block_color) || length(block_color) == length(category),
         "invalid arguments: check that the length of block_color is equal as other inputs"),

    list(is.null(facet_rows) || length(facet_rows) == length(category),
         "invalid arguments: check that the length of block_color is equal as other inputs"),

    list(is.character.single(x_label), "invalid arguments: check that x_label is of type character"),
    list(is.character.single(y_label), "invalid arguments: check that y_label is of type character"),
    list(is.character.single(legend_label), "invalid arguments: check that legend_label is of type character"),
    list(is.character.single(sort_by), "invalid arguments: check that sort_by is of type character"),

    list(sort_by %in% c("count", "alphabetical"), 'invalid arguments: sort_by should be "count" or "alphabetical"'),
    list(block_count %in% c("# of patients", "# of AEs"),
         'invalid arguments: sort_by should be "# of patients" or "# of AEs"')
  )

  # set up data-------
  dat <- data.frame(id = id, y = str_wrap(category, width = 30), r_flag = right_flag, l_flag = left_flag)

  groups <- "y"
  if (!is.null(facet_rows)) {
    facet_rows <- interaction(facet_rows)
    dat <- mutate(dat, f_rows = facet_rows)
    groups <- c(groups, "f_rows")
  }
  if (!is.null(block_color)) {
    dat <- mutate(dat, bar_color = block_color)
    groups <- c(groups, "bar_color")
  }

  if (block_count == "# of patients") {
    counts_r <- dat %>%
      filter(.data$r_flag == 1) %>%
      group_by_(.dots = groups) %>%
      summarize(n_i = length(unique(.data$id))) %>%
      mutate(label_ypos = rev(cumsum(rev(n_i))))

    counts_l <- dat %>%
      filter(.data$l_flag == 1) %>%
      group_by_(.dots = groups) %>%
      summarize(n_i = length(unique(.data$id))) %>%
      mutate(label_ypos = rev(cumsum(rev(n_i))))

  } else if (block_count == "# of AEs") {
    counts_r <- dat %>%
      summarize(n_i = sum(r_flag)) %>%
      mutate(label_ypos = rev(cumsum(rev(n_i))))

    counts_l <- dat %>%
      summarize(n_i = sum(l_flag)) %>%
      mutate(label_ypos = rev(cumsum(rev(n_i))))
  }

  total_text_ann_r <- counts_r %>%
    group_by_(.dots = setdiff(groups, "bar_color")) %>%
    summarize(n = sum(n_i)) %>%
    ungroup()

  total_text_ann_l <- counts_l %>%
    group_by_(.dots = setdiff(groups, "bar_color")) %>%
    summarize(n = sum(n_i)) %>%
    ungroup()

  max_c <- max(c(total_text_ann_r$n, total_text_ann_l$n))

  if (sort_by == "alphabetical") {
    counts <- data.frame(y = c(as.character(counts_r$y), as.character(counts_l$y)))
    counts_r$y <- factor(counts_r$y, levels = sort(total_text_ann_r$y))
    counts_l$y <- factor(counts_l$y, levels = total_text_ann_l$y)
  } else if (sort_by == "count") {
    tot <- bind_rows(total_text_ann_r, total_text_ann_l) %>%
      group_by(y) %>%
      summarize(n = max(n))

    counts_r$y <- factor(counts_r$y, levels = tot$y[order(tot$n)])
    counts_l$y <- factor(counts_l$y, levels = tot$y[order(tot$n)])
  }

  if (is.null(group_names)) {
    g_r <- names(right_flag)[1]
    g_l <- names(left_flag)[2]
  } else {
    g_r <- group_names[1]
    g_l <- group_names[2]
  }

  # plot butterfly plot --------------------
  if (!is.null(block_color)) {
    pl <- ggplot(NULL, aes_string(x = "y")) +
      geom_bar(data = counts_r, aes_string(y = "n_i", fill = "bar_color"), stat = "identity") +
      geom_bar(data = counts_l, aes_string(y = "-n_i", fill = "bar_color"), stat = "identity") +
      geom_hline(yintercept = 0, colour = "black", lwd = 0.4) +
      geom_text(data = counts_r, aes_string(y = "label_ypos - 0.2", label = "n_i"), hjust = 1) +
      geom_text(data = counts_l, aes_string(y = "-label_ypos", label = "n_i"), hjust = -0.9) +
      geom_text(data = total_text_ann_r, aes_string(y = "n", label = "n"), fontface = "bold", hjust = -1) +
      geom_text(data = total_text_ann_l, aes_string(y = "-n - 0.4", label = "n"), fontface = "bold", hjust = 0.9) +
      coord_flip() +
      scale_y_continuous(labels = abs, limits = (max_c * 1.2) * c(-1, 1)) +
      labs(x = y_label, y = block_count, fill = legend_label)
  } else {
    pl <- ggplot(NULL, aes_string(x = "y")) +
      geom_bar(data = counts_r, aes_string(y = "n_i"), stat = "identity") +
      geom_bar(data = counts_l, aes_string(y = "-n_i"), stat = "identity") +
      geom_hline(yintercept = 0, colour = "black", lwd = 0.4) +
      geom_text(data = total_text_ann_r, aes_string(y = "n", label = "n"), fontface = "bold", hjust = -1) +
      geom_text(data = total_text_ann_l, aes_string(y = "-n - 0.4", label = "n"),
                fontface = "bold", hjust = 0.9) +
      coord_flip() +
      scale_y_continuous(labels = abs, limits = (max_c * 1.2) * c(-1, 1)) +
      labs(x = y_label, y = block_count, fill = legend_label)
  }

  if (!is.null(facet_rows)) {
    pl <- pl + facet_wrap(~f_rows, ncol = 1)
  }


  pl <- pl +
    theme_bw() +
    theme(
      strip.background = element_rect(colour = "white", fill = "white"),
      strip.text.x = element_text(color = "black", size = 14),
      title = element_text(size = 9),
      axis.title = element_text(size = 20),
      axis.text = element_text(color = "black", size = 9),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 9),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
      plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
      legend.position = if (show_legend) "right" else "none"
    )

  # labs pl <- pl + labs(title = str_wrap(g2, width = 30))
  g_0 <- ggplotGrob(pl)

  g_1 <- gtable_add_grob(
    g_0,
    grid.text(str_wrap(g_r, width = 30), x = 1, just = "center", hjust = 1, gp = gpar(fontsize = 11)),
    t = 1.5, l = g_0$layout[grep("axis-r", g_0$layout$name)[1], 2], b = 3, name = "right-title", clip = "off"
  )
  g_2 <- gtable_add_grob(
    g_1,
    grid.text(str_wrap(g_l, width = 30), x = 1, just = "center", hjust = 0, gp = gpar(fontsize = 11)),
    t = 1.5, l = g_0$layout[grep("axis-l", g_0$layout$name)[1], 2], b = 3, name = "left-title", clip = "off"
  )
  grid.draw(g_2)
  invisible(g_2)
}
