#' Common Events Plot
#'
#' This function plots commonly occurred events by number of unique subjects with events.
#' It creates basic summary of events and compares event occurrences between treatment and reference
#' arms, and can be used for events data such as Adverse Events.
#'
#' @param term event term vector.
#' @param id id of the event term vector. Ususally it is USUBJID.
#' @param arm Arm vector. Usually it is ACTARM.
#' @param arm_sl Subject level of arm vector. Usually it is adsl$ACTARM.
#' @param trt treatment arm
#' @param ref reference arm
#' @param sort_by sort_by type variable. Default set to "term", to sort_by by term. You can also use
#' "riskdiff", "meanrisk" to sort.
#' @param term_selected selected term to draw common ae. filter term with term_selected if it is not NULL.
#' @param rate_range Range for overall rate
#' @param diff_range Range for rate difference
#' @param reversed Whether to sort_by on reversed variable. Default set to FALSE.
#' @param conf_level The confidence interval level, default set to 0.95.
#' @param ci_method The method used to calculate confidence interval. Defalt is "wald".
#' Possible choices are methods supported in `DescTools::BinomDiffCI`.
#' @param axis_side the side of the axis label, "left" or "right". Default set to "left".
#' @param color Color for the plot. Vector of length 2. Color for arms seperately.
#' Default set to c("red", "blue")
#' @param shape Shape for the plot. Vector of length 2. Shape for arms seperately.
#' Default set to c(16, 17).
#' @param fontsize font size for the plot. It is the size used in ggplot2 with default unit "mm", if you
#' want "points" you will need to devide the point number by "ggplot2:::.pt"
#'
#' @details there is no equivalent STREAM output
#'
#' @return grob object
#'
#' @importFrom dplyr %>% filter mutate tibble distinct group_by summarise ungroup left_join arrange top_n pull
#' @importFrom rlang !! sym :=
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @importFrom tidyr spread gather expand_grid replace_na pivot_wider pivot_longer
#' @importFrom grid textGrob unit unit.c
#' @importFrom DescTools BinomDiffCI
#' @export
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @examples
#' library(osprey)
#' library(random.cdisc.data)
#'
#' term <- as.character(cadae$AEDECOD)
#' id <- cadae$USUBJID
#' arm <- as.character(cadae$ACTARMCD)
#' arm_sl <- as.character(cadsl$ACTARMCD)
#' trt <- "ARM A"
#' ref <- "ARM B"
#' g_events_term_id(term, id, arm, arm_sl, sort_by = "riskdiff")
#' g_events_term_id(term, id, arm, arm_sl, sort_by = "term", reversed = FALSE)
#' g_events_term_id(term, id, arm, arm_sl, sort_by = "meanrisk",
#' reversed = FALSE, axis_side = "right")
#' g_events_term_id(term, id, arm, arm_sl, conf_level = 0.9, fontsize = 6)

g_events_term_id <- function(term,
                             id,
                             arm,
                             arm_sl,
                             trt = unique(arm)[1],
                             ref = unique(arm)[2],
                             sort_by = "term",
                             term_selected = NULL,
                             rate_range = c(0, 1),
                             diff_range = c(-1, 1),
                             reversed = FALSE,
                             conf_level = 0.95,
                             ci_method = "wald",
                             axis_side = "left",
                             color = c("red", "blue"),
                             shape = c(16, 17),
                             fontsize = 4) {
  # argument validation
  possible_sort <- c("term", "riskdiff", "meanrisk")
  possible_axis <- c("left", "right")

  if (is.null(term_selected)) {
    term_selected <- unique(term)
  }
  stop_if_not(
    list(!is_empty(term), "missing argument: term must be specified"),
    list(!is_empty(id), "missing argument: id must be specified"),
    list(!is_empty(arm), "missing argument: arm must be specified"),
    list(
      !is_empty(arm_sl),
      "missing argument: arm_sl must be specified"
    ),

    list(
      length(unique(vapply(
        list(id, term, arm), length, integer(1)
      ))) == 1,
      "invalid arguments: check that the length of id, term and arm are identical"
    ),

    list(
      is_character_vector(arm_sl, min_length = 2),
      "invalid argument: check that arm_sl is a character vector with length >= 2"
    ),
    list(
      all(c(trt, ref) %in% unique(arm)),
      "invalid arguments: trt and ref need to be from arm"
    ),
    list(
      sort_by %in% possible_sort,
      "invalid argument: sort_by should be 'term', 'riskdiff' or 'meanrisk'"
    ),
    list(
      axis_side %in% possible_axis,
      "invalid argument: axis_side should be 'left' or 'right'"
    ),
    list(
      is.null(term_selected) | term_selected %in% unique(term),
      "invalid argument: term_selected should be NULL or from term"
    ),
    list(
      is_numeric_vector(rate_range, min_length = 2, max_length = 2),
      "invalid argument: rate_range should be a numeric vector of length 2"
    ),
    list(
      is_numeric_vector(diff_range, min_length = 2, max_length = 2),
      "invalid argument: diff_range should be a numeric vector of length 2"
    ),
    list(
      is_logical_single(reversed),
      "invalid argument: reversed should be a TRUE or FALSE"
    ),
    list(
      is_character_single(ci_method) &
        ci_method %in% c(
          "wald",
          "waldcc",
          "ac",
          "scorecc",
          "score",
          "mn",
          "mee",
          "blj",
          "ha"
        ),
      "invalid argument: ci_method should be a method supported by `DescTools::BinomDiffCI`"
    ),
    list(
      is_numeric_single(conf_level) & between(conf_level, 0.5, 1),
      "invalid argument: conf_level should be a number between 0.5 and 1"
    ),
    list(
      is_character_vector(color, min_length = 2, max_length = 2),
      "invalid argument: check that color is a character vector of length 2"
    ),
    list(
      is_numeric_vector(shape, min_length = 2, max_length = 2),
      "invalid argument: check that shape is a numeric vector of length 2"
    ),
    list(
      is_numeric_single(fontsize) & between(fontsize, 0, Inf),
      "invalid argument: check that fontsize is a number greater than 0"
    )
  )

  arms <- c(trt, ref)
  n <- tibble(arm = arm_sl) %>%
    group_by(arm) %>%
    summarise(total = n()) %>%
    mutate(label = sprintf("%s (N=%i)", arm, total),
           label = str_wrap(label, width = 10)) %>%
    filter(arm %in% arms)

  df <- tibble(id = id, arm = arm, term = term) %>%
    filter(arm %in% arms & term %in% term_selected) %>%
    distinct() %>%
    group_by(arm, term) %>%
    summarise(count = n()) %>%
    ungroup

  names(color) <- arms
  names(shape) <- arms

  xs <- syms(paste("count", arms, sep = "__"))
  ns <- syms(paste("total", arms, sep = "__"))
  rs <- syms(paste("risk", arms, sep = "__"))

  x1 <- xs[[1]]
  n1 <- ns[[1]]
  x2 <- xs[[2]]
  n2 <- ns[[2]]
  r1 <- rs[[1]]
  r2 <- rs[[2]]

  df_ref <- expand_grid(term = term_selected, arm = arms)

  df_risk <- df %>%
    select(term, count, arm) %>%
    full_join(df_ref, by = c("term", "arm")) %>%
    tidyr::replace_na(list(count = 0)) %>%
    mutate(tmp = 1) %>%
    pivot_wider(
      values_from = "count",
      names_from = "arm",
      values_fill = list("count" = 0),
      names_prefix = "count__"
    ) %>%
    left_join(
      n %>%
        select(arm, total) %>%
        pivot_wider(
          names_from = "arm",
          values_from = "total",
          names_prefix = "total__"
        ) %>%
        mutate(tmp = 1),
      by = "tmp"
    ) %>%
    select(-tmp) %>%
    group_by(term) %>%
    mutate(
      lower = BinomDiffCI(!!x1,!!n1,!!x2,!!n2,
                          conf_level, method = ci_method)[2],
      upper = BinomDiffCI(!!x1,!!n1,!!x2,!!n2,
                          conf_level, method = ci_method)[3],!!r1 := !!x1 / !!n1,!!r2 := !!x2 / !!n2,
      riskdiff = !!r1-!!r2,
      meanrisk = (!!x1+!!x2) / (!!n1+!!n2)
    ) %>%
    ungroup %>%
    filter(meanrisk > rate_range[1] & meanrisk < rate_range[2]) %>%
    filter(riskdiff > diff_range[1] & riskdiff < diff_range[2])
  if (nrow(df_risk) == 0) {
    grid.draw(textGrob("All Observations are filtered out"))
    return(NULL)
  }
  if (sort_by != "term") {
    sort_var <- sym(sort_by)
    df_risk <-  df_risk %>%
      arrange(!!sort_var)
  } else {
    df_risk <-  df_risk %>%
      arrange(desc(term))
  }


  terms_needed <- df_risk$term

  if (reversed) {
    terms_needed <- rev(terms_needed)
  }


  terms_needed <- terms_needed
  terms_label <- sapply(lapply(terms_needed, strwrap, width = 30),
                        paste, collapse = "\n")

  df_risk <- df_risk %>%
    pivot_longer(matches("__"),
                 names_to = c(".value", "arm"),
                 names_sep = "__")

  mytheme <-
    theme_osprey(axis_side = axis_side, fontsize = fontsize)

  labels <- n$label
  names(labels) <- n$arm

  y_axis <-
    scale_y_discrete(
      limits = terms_needed,
      breaks = terms_needed,
      labels = terms_label,
      position = axis_side
    )

  p1 <- ggplot(df_risk) +
    geom_point(aes(
      y = term,
      x = risk,
      group = arm,
      color = arm,
      shape = arm
    ),
    size = fontsize * 0.7) +
    mytheme +
    ggtitle("Proportion") +
    scale_color_manual(values = color, labels = labels) +
    scale_shape_manual(values = shape, labels = labels) +
    y_axis

  p2 <- ggplot(df_risk) +
    geom_point(mapping = aes(y = term, x = riskdiff),
               size = fontsize * 0.7) +
    geom_vline(data = NULL,
               xintercept = 0,
               linetype = 2) +
    mytheme +
    geom_errorbarh(mapping = aes(xmax = upper, xmin = lower, y = term),
                   height = 0.4) +
    y_axis +
    ggtitle("Risk Difference")

  axis_name <- sprintf("axis-%s", substr(axis_side, 1, 1))
  p1_parts <- ggplotGrob(p1)

  p2_parts <- ggplotGrob(p2)


  mylegend <- grob_part(grob_part(p1_parts, "guide-box"), "guides")
  axis <- grob_part(p1_parts, axis_name)

  less_risk <- textGrob(
    "Favor\nTreatment",
    just = "centre",
    x = unit(fontsize * .pt, "pt"),
    gp = gpar(fontsize = fontsize * .pt, fontface = "bold")
  )
  more_risk <- textGrob(
    "Favor\nControl",
    just = "centre",
    x = unit(1, "npc") - unit(fontsize * .pt, "pt"),
    gp = gpar(fontsize = fontsize * .pt, fontface = "bold")
  )

  risk_label <- arrangeGrob(less_risk, more_risk, nrow = 1)
  title1 <- grob_part(p1_parts, "title")
  title2 <- grob_part(p2_parts, "title")
  panel1 <- grob_part(p1_parts, "panel")
  panel2 <- grob_part(p2_parts, "panel")
  axis_b1 <- grob_part(p1_parts, "axis-b")
  axis_b2 <- grob_part(p2_parts, "axis-b")

  grobs <- list(title1,
                title2,
                axis,
                panel1,
                panel2,
                axis_b1,
                axis_b2,
                mylegend,
                risk_label)

  if (axis_side == "left") {
    layout_matrix <- rbind(c(NA, 1, NA, 2),
                           c(3, 4, NA, 5),
                           c(NA, 6, NA, 7),
                           c(8, 8, NA, 9))
    widths <- unit.c(grobWidth(axis), unit(c(1, 2 * fontsize, 1),
                                           c("null", "pt", "null")))

  } else{
    layout_matrix <- rbind(c(1, NA, 2, NA),
                           c(4, NA, 5, 3),
                           c(6, NA, 7, NA),
                           c(8, NA, 9, NA))
    widths <- unit.c(unit(c(1, 10, 1),
                          c("null", "pt", "null")),
                     grobWidth(axis))
  }
  heights <-
    unit.c(
      grobHeight(title1),
      unit(1, "null"),
      grobHeight(axis_b1),
      max(grobHeight(mylegend), grobHeight(more_risk))
    )

  ret <- arrangeGrob(
    grobs = grobs,
    nrow = 4,
    ncol = 4,
    layout_matrix = layout_matrix,
    heights = heights,
    widths = widths
  )

  ret <- grob_add_padding(ret)
  plot(ret)
  invisible(ret)
}
