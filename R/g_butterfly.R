#' Butterfly Plot
#'
#' The butterfly plot is often used in Early Development (ED) and is an opposed
#' barplot that shows instances of AEs or # of patients by category separated by
#' a dichotomization variable. Each bar can be color coded according
#' to a variable of choice and sorted according to either alphabetical order or the
#' maximum count.
#'
#' @param category vector of y values
#' @param right_flag vector of \code{logical} of the same length as \code{category}.
#'   used to filter \code{category} for the right side of the barplot.
#'   to maintain backward compatibility, a vector of 1s and 0s would also work.
#' @param left_flag vector of \code{logical} of the same length as \code{category}.
#'   used to filter \code{category} for the left side of the barplot.
#'   to maintain backward compatibility, a vector of 1s and 0s would also work.
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
#' options: "alphabetical", "count", "left", "right", default here is set to "count"
#' @param show_legend boolean of whether color coding legend is included,
#' default here is FALSE
#'
#' @details there is no equivalent STREAM output
#'
#' @return ggplot object
#'
#' @export
#'
#' @template author_zhanc107
#' @template author_qit3
#'
#' @examples
#' library(dplyr)
#' library(nestcolor)
#'
#' ADSL <- rADSL %>%
#'   select(USUBJID, STUDYID, SEX, ARM, RACE) %>%
#'   dplyr::filter(SEX %in% c("F", "M"))
#' AAE <- rADAE %>% select(USUBJID, STUDYID, AEBODSYS, AETOXGR)
#'
#' ANL <- left_join(AAE, ADSL, by = c("STUDYID", "USUBJID"))
#' ANL <- ANL %>%
#'   dplyr::mutate(flag1 = ifelse(RACE == "ASIAN", 1, 0)) %>%
#'   dplyr::mutate(flag2 = ifelse(SEX == "M", 1, 0))
#' ANL <- na.omit(ANL)
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
g_butterfly <- function(category,
                        right_flag,
                        left_flag,
                        id = NULL,
                        group_names = NULL,
                        block_count = c("# of patients", "# of AEs"),
                        block_color = NULL,
                        facet_rows = NULL,
                        x_label = block_count,
                        y_label = "AE Derived Terms",
                        legend_label = "AETOXGR",
                        sort_by = c("count", "alphabetical", "right", "left"),
                        show_legend = TRUE) {
  stopifnot(
    "invalid arguments: check that the length of input arguments are identical" =
      length(category) == length(right_flag) && length(right_flag) == length(left_flag)
  )
  stopifnot(
    "invalid arguments: right_flag or left_flag contains values other than 1/TRUE or 0/FALSE" =
      all(union(right_flag, left_flag) %in% c(1, 0))
  )
  stopifnot(
    "invalid arguments: right_flag and left_flag contain only 0/FALSE values" =
      any(union(right_flag, left_flag) == 1)
  )
  stopifnot(
    "invalid arguments: check that the length of block_color is equal as other inputs" =
      is.null(block_color) || length(block_color) == length(category)
  )
  block_count <- match.arg(block_count)
  checkmate::assert_character(id, null.ok = isFALSE(block_count == "# of patients"))

  stopifnot(
    "invalid arguments: check that the length of block_color is equal as other inputs" =
      is.null(id) || length(id) == length(category)
  )
  stopifnot(
    "invalid arguments: check that the length of block_color is equal as other inputs" =
      is.null(facet_rows) ||
        (length(facet_rows) == length(category)) ||
        (is.data.frame(facet_rows) && nrow(facet_rows) == length(category))
  )
  checkmate::assert_string(x_label)
  checkmate::assert_string(y_label)
  checkmate::assert_string(legend_label)
  sort_by <- match.arg(sort_by)

  # set up data-------
  dat <- data.frame(y = stringr::str_wrap(category, width = 30), r_flag = right_flag, l_flag = left_flag)

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
        dplyr::filter(.data$bar_color == sort(.data$bar_color, decreasing = TRUE)[1])
    } else {
      .data
    }
  }

  counts_r <- dat %>%
    dplyr::filter(.data$r_flag == 1) %>%
    highest_grade(block_count) %>%
    dplyr::group_by_at(groups) %>%
    dplyr::summarize(n_i = get_counts(.data, block_count)) %>%
    dplyr::group_by_at(setdiff(groups, "bar_color")) %>%
    dplyr::mutate(label_ypos = rev(cumsum(rev(.data$n_i))))

  counts_l <- dat %>%
    filter(.data$l_flag == 1) %>%
    highest_grade(block_count) %>%
    dplyr::group_by_at(groups) %>%
    dplyr::summarize(n_i = get_counts(.data, block_count)) %>%
    dplyr::group_by_at(setdiff(groups, "bar_color")) %>%
    dplyr::mutate(label_ypos = rev(cumsum(rev(.data$n_i))))

  total_label_pos_r <- counts_r %>%
    dplyr::group_by_at(setdiff(groups, "bar_color")) %>%
    dplyr::summarize(label_ypos = max(.data$label_ypos))

  total_label_pos_l <- counts_l %>%
    dplyr::group_by_at(setdiff(groups, "bar_color")) %>%
    dplyr::summarize(label_ypos = max(.data$label_ypos))

  total_text_ann_r <- dat %>%
    dplyr::filter(.data$r_flag == 1) %>%
    dplyr::group_by_at(setdiff(groups, "bar_color")) %>%
    dplyr::summarize(n = get_counts(.data, block_count)) %>%
    dplyr::left_join(total_label_pos_r, by = setdiff(groups, "bar_color"))

  total_text_ann_l <- dat %>%
    dplyr::filter(.data$l_flag == 1) %>%
    dplyr::group_by_at(setdiff(groups, "bar_color")) %>%
    dplyr::summarize(n = get_counts(.data, block_count)) %>%
    dplyr::left_join(total_label_pos_l, by = setdiff(groups, "bar_color"))


  if (sort_by == "alphabetical") {
    levels_all <- unique(sort(as.character(union(counts_l$y, counts_r$y)), decreasing = TRUE))
    counts_r$y <- factor(counts_r$y, levels = levels_all)
    counts_l$y <- factor(counts_l$y, levels = levels_all)
  } else if (sort_by == "count") {
    tot <- dplyr::bind_rows(total_text_ann_r, total_text_ann_l) %>%
      dplyr::group_by(.data$y) %>%
      dplyr::summarize(n = sum(n)) %>%
      dplyr::arrange(n)

    counts_r$y <- factor(counts_r$y, levels = tot$y)
    counts_l$y <- factor(counts_l$y, levels = tot$y)
  } else if (sort_by == "right") {
    tot <- dplyr::full_join(total_text_ann_r, select(total_text_ann_l, -n), by = "y") %>%
      dplyr::group_by(.data$y) %>%
      dplyr::summarize(n = sum(n, na.rm = TRUE)) %>%
      dplyr::arrange(n)

    counts_r$y <- factor(counts_r$y, levels = tot$y)
    counts_l$y <- factor(counts_l$y, levels = tot$y)
  } else if (sort_by == "left") {
    tot <- dplyr::full_join(total_text_ann_l, select(total_text_ann_r, -n), by = "y") %>%
      dplyr::group_by(.data$y) %>%
      dplyr::summarize(n = sum(n, na.rm = TRUE)) %>%
      dplyr::arrange(n)

    counts_r$y <- factor(counts_r$y, levels = tot$y)
    counts_l$y <- factor(counts_l$y, levels = tot$y)
  }

  max_c <- max(c(total_text_ann_r$label_ypos, total_text_ann_l$label_ypos))

  if (is.null(group_names)) {
    g_r <- ""
    g_l <- ""
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
      geom_text(
        data = total_text_ann_l, aes_string(y = "-label_ypos - 0.4", label = "n"),
        fontface = "bold", hjust = 0.9
      ) +
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
      geom_text(
        data = total_text_ann_l, aes_string(y = "-label_ypos - 0.4", label = "n"),
        fontface = "bold", hjust = 0.9
      ) +
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
      plot.margin = grid::unit(c(1.5, 1, 1, 1), "cm"),
      legend.position = if (show_legend) "right" else "none"
    ) +
    scale_x_discrete(limits = levels(counts_r$y))

  # labs pl <- pl + labs(title = stringr::str_wrap(g2, width = 30))
  g_0 <- ggplotGrob(pl)

  g_1 <- gtable::gtable_add_grob(
    g_0,
    grid::grid.text(
      stringr::str_wrap(g_r, width = 30),
      x = 1, just = "center", hjust = 1, gp = grid::gpar(fontsize = 11)
    ),
    t = 1.5, l = g_0$layout[grep("axis-r", g_0$layout$name)[1], 2], b = 3, name = "right-title", clip = "off"
  )
  g_2 <- gtable::gtable_add_grob(
    g_1,
    grid::grid.text(
      stringr::str_wrap(g_l, width = 30),
      x = 1, just = "center", hjust = 0, gp = grid::gpar(fontsize = 11)
    ),
    t = 1.5, l = g_0$layout[grep("axis-l", g_0$layout$name)[1], 2], b = 3, name = "left-title", clip = "off"
  )
  grid::grid.draw(g_2)
  invisible(g_2)
}
