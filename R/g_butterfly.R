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
#' library(osprey)
#'
#' ADSL <- rADSL %>% select(USUBJID, STUDYID, SEX, ARM, RACE) %>% dplyr::filter(SEX %in% c("F", "M"))
#' AAE <- rADAE %>% select(USUBJID, STUDYID, AEBODSYS, AETOXGR)
#'
#' ANL <- left_join(AAE, ADSL, by = c("STUDYID", "USUBJID"))
#' ANL <- ANL %>%
#'   dplyr::mutate(flag1 = ifelse(RACE == "ASIAN", 1, 0)) %>%
#'   dplyr::mutate(flag2 = ifelse(SEX == "M", 1, 0))
#' ANL <- na.omit(ANL)
#' ANL <- ANL %>% dplyr::filter(AEBODSYS %in% c(
#'   "Investigations", "Vascular disorders",
#'   "Musculoskeletal and connective tissue disorders"
#' ))
#'
#' # Example 1, # of AEs
#' g_butterfly(
#'   category = ANL$AEBODSYS,
#'   right_flag = ANL$flag1,
#'   left_flag = ANL$flag2,
#'   group_names = c("flag1 Asian", "flag2 M"),
#'   block_count = "# of AEs",
#'   block_color = ANL$AETOXGR,
#'   id = ANL$USUBJID,
#'   x_label = "# of AEs",
#'   y_label = "AE Body System",
#'   legend_label = "AETOXGR",
#'   sort_by = "count",
#'   show_legend = TRUE
#' )
#'
#' # Example 2, # of patients with facet
#' g_butterfly(
#'   category = ANL$AEBODSYS,
#'   right_flag = ANL$flag1,
#'   left_flag = ANL$flag2,
#'   group_names = c("flag1 Asian", "flag2 M"),
#'   block_count = "# of patients",
#'   block_color = ANL$AETOXGR,
#'   facet_rows = ANL$ARM,
#'   id = ANL$USUBJID,
#'   x_label = "# of patients",
#'   y_label = "AE Derived Terms",
#'   legend_label = "AETOXGR",
#'   sort_by = "count",
#'   show_legend = TRUE
#' )
#'
g_butterfly <- function(category,
                        right_flag,
                        left_flag,
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

    list(length(unique(vapply(list(category, right_flag, left_flag), length, integer(1)))) == 1,
         "invalid arguments: check that the length of input arguments are identical"),

    list(length(unique(right_flag)) == 2 && length(unique(left_flag)),
         "invalid arguments: groups can only have 2 unique values"),

    list(is.null(block_color) || length(block_color) == length(category),
         "invalid arguments: check that the length of block_color is equal as other inputs"),

    list(block_count %in% c("# of patients", "# of AEs"),
         'invalid arguments: sort_by should be "# of patients" or "# of AEs"'),
    list(!(block_count == "# of patients" && is.null(id)),
         "invalid arguments: for '# of patients' id have to be specified"),
    list(is.null(id) || length(id) == length(category),
         "invalid arguments: check that the length of block_color is equal as other inputs"),

    list(is.null(facet_rows) ||
        (length(facet_rows) == length(category)) ||
        (is.data.frame(facet_rows) && nrow(facet_rows) == length(category)),
         "invalid arguments: check that the length of block_color is equal as other inputs"),

    list(is.character.single(x_label), "invalid arguments: check that x_label is of type character"),
    list(is.character.single(y_label), "invalid arguments: check that y_label is of type character"),
    list(is.character.single(legend_label), "invalid arguments: check that legend_label is of type character"),
    list(is.character.single(sort_by), "invalid arguments: check that sort_by is of type character"),

    list(sort_by %in% c("count", "alphabetical"), 'invalid arguments: sort_by should be "count" or "alphabetical"')
  )

  # set up data-------
  dat <- data.frame(y = str_wrap(category, width = 30), r_flag = right_flag, l_flag = left_flag)

  groups <- "y"

  if (!is.null(id)) {
    dat$id <- id
  }
  if (!is.null(facet_rows)) {
    facet_rows <- interaction(facet_rows)
    dat$f_rows <- facet_rows
    groups <- c(groups, "f_rows")
  }
  if (!is.null(block_color)) {
    dat$bar_color <- block_color
    groups <- c(groups, "bar_color")
  }

  get_counts <- function(.data, block_count) {
    if (block_count == "# of patients") {
      length(unique(.data$id))
    } else if (block_count == "# of AEs") {
      n()
    }
  }
  highest_grade <- function(.data, block_count) {
    if (block_count == "# of patients" && "bar_color" %in% colnames(.data)) {
      .data %>%
      dplyr::group_by(.data$y, .data$id) %>%
        filter(.data$bar_color == max(.data$bar_color, na.rm = TRUE))
    } else {
      .data
    }
  }

  counts_r <- dat %>%
    filter(.data$r_flag == 1) %>%
    highest_grade(block_count) %>%
    group_by_(.dots = groups) %>%
    summarize(n_i = get_counts(.data, block_count)) %>%
    group_by_(.dots = setdiff(groups, "bar_color")) %>%
    mutate(label_ypos = rev(cumsum(rev(.data$n_i))))

  counts_l <- dat %>%
    filter(.data$l_flag == 1) %>%
    highest_grade(block_count) %>%
    group_by_(.dots = groups) %>%
    summarize(n_i = get_counts(.data, block_count)) %>%
    group_by_(.dots = setdiff(groups, "bar_color")) %>%
    mutate(label_ypos = rev(cumsum(rev(.data$n_i))))

  total_label_pos_r <- counts_r %>%
    group_by_(.dots = setdiff(groups, "bar_color")) %>%
    summarize(label_ypos = max(.data$label_ypos))

  total_label_pos_l <- counts_l %>%
    group_by_(.dots = setdiff(groups, "bar_color")) %>%
    summarize(label_ypos = max(.data$label_ypos))

  total_text_ann_r <- dat %>%
    filter(.data$r_flag == 1) %>%
    group_by_(.dots = setdiff(groups, "bar_color")) %>%
    summarize(n = get_counts(.data, block_count)) %>%
    left_join(total_label_pos_r, by = setdiff(groups, "bar_color"))

  total_text_ann_l <- dat %>%
    filter(.data$l_flag == 1) %>%
    group_by_(.dots = setdiff(groups, "bar_color")) %>%
    summarize(n = get_counts(.data, block_count)) %>%
    left_join(total_label_pos_l, by = setdiff(groups, "bar_color"))


  if (sort_by == "alphabetical") {
    counts_r$y <- factor(counts_r$y, levels = unique(sort(as.character(counts_r$y), decreasing = TRUE)))
    counts_l$y <- factor(counts_l$y, levels = unique(sort(as.character(counts_l$y), decreasing = TRUE)))
  } else if (sort_by == "count") {
    tot <- bind_rows(total_text_ann_r, total_text_ann_l) %>%
      group_by(.data$y) %>%
      summarize(n = sum(n)) %>%
      arrange(n)

    counts_r$y <- factor(counts_r$y, levels = tot$y)
    counts_l$y <- factor(counts_l$y, levels = tot$y)
  }

  max_c <- max(c(total_text_ann_r$label_ypos, total_text_ann_l$label_ypos))

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
      geom_text(data = counts_r, aes_string(y = "label_ypos", label = "n_i"), hjust = 0.9) +
      geom_text(data = counts_l, aes_string(y = "-label_ypos", label = "n_i"), hjust = -0.9) +
      geom_text(data = total_text_ann_r, aes_string(y = "label_ypos", label = "n"), fontface = "bold", hjust = -1) +
      geom_text(data = total_text_ann_l, aes_string(y = "-label_ypos - 0.4", label = "n"),
                fontface = "bold", hjust = 0.9) +
      geom_hline(yintercept = 0, colour = "black", lwd = 0.4) +
      coord_flip() +
      scale_y_continuous(labels = abs, limits = (max_c * 1.2) * c(-1, 1)) +
      labs(x = y_label, y = block_count, fill = legend_label)
  } else {
    pl <- ggplot(NULL, aes_string(x = "y")) +
      geom_bar(data = counts_r, aes_string(y = "n_i"), stat = "identity") +
      geom_bar(data = counts_l, aes_string(y = "-n_i"), stat = "identity") +
      geom_hline(yintercept = 0, colour = "black", lwd = 0.4) +
      geom_text(data = total_text_ann_r, aes_string(y = "label_ypos", label = "n"), fontface = "bold", hjust = -1) +
      geom_text(data = total_text_ann_l, aes_string(y = "-label_ypos - 0.4", label = "n"),
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
