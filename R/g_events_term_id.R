#' Events by Term Plot
#'
#' This function plots commonly occurred events by number of unique subjects with events.
#' It creates basic summary of events and compares event occurrences between comparison
#' and reference arms, and can be used for events data such as Adverse Events.
#'
#' @inheritParams argument_convention
#' @param id (`vector`)\cr contains subject identifier. Length of \code{id} must be the
#' same as the length or number of rows of \code{terms}. Usually it is \code{ADAE$USUBJID}.
#' @param term \code{character} or \code{factor} vector, or \code{data.frame} \cr
#' Represents events information. \code{term} can be a \code{data.frame} produced
#' by \code{create_flag_vars}, with each column being a \code{logical} event indicator
#' @param arm_N (\code{numeric} vector)\cr
#' Contains information of the number of patients in the levels of \code{arm}. This is useful
#' if there are patients that have no adverse events can be accounted for with this argument.
#' @param ref \code{character} indicates the name of the reference arm. Default is the first
#' level of \code{arm}.
#' @param trt \code{character} indicates the name of the treatment arm. Default is the second
#' level of \code{arm}.
#' @param sort_by \code{character} indicates how each \code{term} is sorted in the plot.
#' Choose from "term" for alphabetic terms, "riskdiff" for risk difference, and "meanrisk"
#' for mean risk. Default is "term".
#' @param rate_range Numeric \code{vector} of length 2. Range for overall rate display
#' @param diff_range Numeric \code{vector} of length 2. Range for rate difference display
#' @param reversed \code{logical} whether to reverse the sorting by \code{sort_by}.
#' Default is FALSE.
#' @param axis_side \code{character} the side of the axis label, "left" or "right". Default is "left".
#' @param color Color for the plot. \code{vector} of length 2. Color for reference and
#' treatment arms respectively. Default set to \code{c("blue", "red")}.
#' @param shape Shape for the plot. \code{vector} of length 2. Shape for reference and
#' treatment arms respectively. Default set to \code{c(16, 17)} per
#' \code{\link[ggplot2]{scale_shape}}.
#' @details there is no equivalent STREAM output
#'
#' @return grob object
#'
#' @import ggplot2
#' @importFrom tidyr pivot_wider unnest
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid textGrob unit unit.c grobHeight grobWidth
#' @importFrom DescTools BinomDiffCI
#' @importFrom stats setNames
#' @export
#'
#' @author Liming Li (Lil128) \email{liming.li@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#'
#' @examples
#' library(scda)
#' library(rtables)
#' library(dplyr)
#' library(grid)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' # add additional dummy causality flags
#' ADAE <- ADAE %>%
#'   mutate(AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X")) %>%
#'   mutate(AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")) %>%
#'   rtables::var_relabel(
#'     AEREL1 = "AE related to A: Drug X",
#'     AEREL2 = "AE related to B: Placebo"
#'   )
#'
#' term <- ADAE$AEDECOD
#' id <- ADAE$USUBJID
#' arm <- ADAE$ACTARMCD
#' arm_N <- table(ADSL$ACTARMCD)
#' ref <- "ARM A"
#' trt <- "ARM C"
#' # Example 1
#' p1 <- g_events_term_id(
#'   term,
#'   id,
#'   arm,
#'   arm_N
#' )
#' grid.newpage()
#' grid.draw(p1)
#'
#' # Example 2
#' p2 <- g_events_term_id(
#'   term,
#'   id,
#'   arm,
#'   arm_N,
#'   trt = trt,
#'   ref = ref,
#'   sort_by = "riskdiff",
#'   diff_ci_method = "ac",
#'   conf_level = 0.9
#' )
#' grid.newpage()
#' grid.draw(p2)
#'
#' # Example 3
#' p3 <- g_events_term_id(
#'   term,
#'   id,
#'   arm,
#'   arm_N,
#'   sort_by = "meanrisk",
#'   axis_side = "right",
#'   fontsize = 5
#' )
#' grid.newpage()
#' grid.draw(p3)
#'
#' # Example 4
#' term <- create_flag_vars(ADAE)
#' g_events_term_id(
#'   term,
#'   id,
#'   arm,
#'   arm_N,
#'   fontsize = 3
#' )
g_events_term_id <- function(term,
                             id,
                             arm,
                             arm_N, # nolint
                             ref = levels(arm)[1],
                             trt = levels(arm)[2],
                             sort_by = c("term", "riskdiff", "meanrisk"),
                             rate_range = c(0, 1),
                             diff_range = c(-1, 1),
                             reversed = FALSE,
                             conf_level = 0.95,
                             diff_ci_method =
                               c("wald", "waldcc", "ac", "score", "scorecc", "mn", "mee", "blj", "ha", "beal"),
                             axis_side = c("left", "right"),
                             color = c("blue", "red"),
                             shape = c(16, 17),
                             fontsize = 4,
                             draw = TRUE) {
  if (is.data.frame(term)) {
    term_levels <- factor(colnames(term), levels = rev(colnames(term)))
    term <- data.frame(t(term))
    term <- lapply(term, function(x) {
      term_levels[x]
    })
    df <- tibble::tibble(id, arm, term) %>%
      tidyr::unnest(term)
  } else {
    term_levels <- `if`(is.factor(term), levels(term), unique(term))
    df <- tibble::tibble(id, arm, term)
  }

  # argument validation
  sort_by <- match.arg(sort_by)
  diff_ci_method <- match.arg(diff_ci_method)
  axis_side <- match.arg(axis_side)

  checkmate::assert_factor(arm, min.levels = 2)
  stopifnot(
    "invalid arguments: check that the length of id, term and arm are identical" =
      length(id) == length(term) && length(term) == length(arm)
  )
  stopifnot("invalid arguments: trt and ref need to be from arm" = all(c(trt, ref) %in% unique(arm)))
  checkmate::assert_numeric(rate_range, len = 2, any.missing = FALSE)
  checkmate::assert_numeric(diff_range, len = 2, any.missing = FALSE)
  checkmate::assert_flag(reversed)
  checkmate::assert_number(conf_level, lower = 0.5, upper = 1)
  checkmate::assert_character(color, len = 2, any.missing = FALSE)
  checkmate::assert_numeric(shape, len = 2, any.missing = FALSE)
  checkmate::assert_numeric(fontsize, lower = 0, any.missing = FALSE)

  # construct calculations
  arms <- c(ref, trt)
  df_n <- data.frame(arm_N)
  names(df_n) <- c("arm", "total")
  df_n <- df_n[df_n$arm %in% arms, ]
  ref_total <- df_n %>%
    filter(arm == ref) %>%
    select(.data$total) %>%
    pull()
  trt_total <- df_n %>%
    filter(arm == trt) %>%
    select(.data$total) %>%
    pull()

  # create full cartesian of (arms) X (term levels) with 0 count to have full list of all possible combination
  # this is done in order to secure further calls
  df_full <- cbind(expand.grid(arm = arms, term = term_levels), N = 0)

  df_reshaped <- df %>%
    distinct() %>%
    filter(arm %in% arms) %>%
    group_by(arm, term) %>%
    summarise(N = n()) %>%
    ungroup() %>%
    rbind(df_full) %>%
    group_by(arm, term) %>%
    summarise(N = sum(.data$N)) %>%
    mutate(arm = ifelse(arm == trt, "trt_count", "ref_count")) %>%
    tidyr::pivot_wider(names_from = arm, values_from = .data$N, values_fill = list(N = 0))

  df_ci <- df_reshaped %>%
    group_by(term) %>%
    do(
      data.frame(
        t(c(
          BinomDiffCI(
            .data$trt_count,
            trt_total,
            .data$ref_count,
            ref_total,
            conf_level,
            method = diff_ci_method
          )[1, ], # wald
          meanrisk = (.data$trt_count + .data$ref_count) / (trt_total + ref_total)
        ))
      )
    ) %>%
    ungroup() %>%
    rename(riskdiff = .data$est, lower_ci = .data$lwr.ci, upper_ci = .data$upr.ci)

  df_risk <- df_reshaped %>%
    group_by(term) %>%
    do(
      data.frame(
        risk = c(.data$trt_count / trt_total, .data$ref_count / ref_total),
        arm = c(trt, ref)
      )
    ) %>%
    ungroup()

  names(color) <- arms
  names(shape) <- arms

  # if diff_range specified, limit terms
  terms_needed <- df_ci %>%
    filter(
      .data$riskdiff > diff_range[1] &
        .data$riskdiff < diff_range[2] &
        .data$meanrisk > rate_range[1] &
        .data$meanrisk < rate_range[2]
    ) %>%
    select(term) %>%
    distinct() %>%
    pull() %>%
    as.character()

  if (length(terms_needed) == 0) {
    ret <- textGrob("All Observations are filtered out")
    if (draw) {
      grid.draw(ret)
    }
    return(invisible(ret))
  }

  # sorting
  if (sort_by == "term") {
    terms_needed <- sort(terms_needed, decreasing = TRUE)
  } else {
    terms_needed <- df_ci %>%
      arrange(.data[[sort_by]]) %>%
      filter(term %in% terms_needed) %>%
      select(term) %>%
      pull()
  }

  if (reversed) {
    terms_needed <- rev(terms_needed)
  }

  df_ci <- df_ci %>%
    filter(term %in% terms_needed) %>%
    mutate(term = factor(term, terms_needed))
  df_risk <- df_risk %>%
    filter(term %in% terms_needed) %>%
    mutate(term = factor(term, terms_needed))
  terms_label <- vapply(
    lapply(terms_needed, strwrap, width = 30),
    paste,
    FUN.VALUE = character(1),
    collapse = "\n"
  )

  mytheme <- theme_osprey(axis_side = axis_side, fontsize = fontsize)

  labels <- setNames(sprintf("%s\n(N = %i)", df_n$arm, df_n$total), df_n$arm)

  y_axis <- scale_y_discrete(
    limits = terms_needed,
    breaks = terms_needed,
    labels = terms_label,
    position = axis_side
  )

  p1 <- ggplot(df_risk) +
    geom_point(aes(
      y = term,
      x = .data$risk,
      group = arm,
      color = arm,
      shape = arm
    ),
    size = fontsize * 0.7
    ) +
    mytheme +
    ggtitle("Proportion") +
    scale_color_manual(values = color, labels = labels) +
    scale_shape_manual(values = shape, labels = labels) +
    y_axis

  p2 <- ggplot(df_ci) +
    geom_point(mapping = aes(y = term, x = .data$riskdiff), size = fontsize * 0.7) +
    geom_vline(data = NULL, xintercept = 0, linetype = 2) +
    mytheme +
    geom_errorbarh(mapping = aes(xmax = .data$upper_ci, xmin = .data$lower_ci, y = term), height = 0.4) +
    y_axis +
    ggtitle("Risk Difference")

  axis_name <- sprintf("axis-%s", substr(axis_side, 1, 1))

  p1_parts <- ggplotGrob(p1)
  p2_parts <- ggplotGrob(p2)

  mylegend <- grob_part(grob_part(p1_parts, "guide-box"), "guides")
  axis <- grob_part(p1_parts, axis_name)

  less_risk <- textGrob(
    "Favor\nTreatment",
    just = "left",
    x = unit(fontsize * .pt, "pt"),
    gp = gpar(fontsize = fontsize * .pt, fontface = "bold")
  )
  more_risk <- textGrob(
    "Favor\nControl",
    just = "right",
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

  grobs <- list(
    title1,
    title2,
    axis,
    panel1,
    panel2,
    axis_b1,
    axis_b2,
    mylegend,
    risk_label
  )

  if (axis_side == "left") {
    layout_matrix <- rbind(
      c(NA, 1, NA, 2),
      c(3, 4, NA, 5),
      c(NA, 6, NA, 7),
      c(8, 8, NA, 9)
    )
    widths <- unit.c(grobWidth(axis), unit(c(1, 2 * fontsize, 1), c("null", "pt", "null")))
  } else {
    layout_matrix <- rbind(
      c(1, NA, 2, NA),
      c(4, NA, 5, 3),
      c(6, NA, 7, NA),
      c(8, NA, 9, NA)
    )
    widths <- unit.c(unit(c(1, 10, 1), c("null", "pt", "null")), grobWidth(axis))
  }

  heights <- unit.c(
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

  if (draw) {
    grid.draw(ret)
  }
  invisible(ret)
}


#' create AE overview flags
#' @param df data frame of AE
#' @param fatal AE with fatal outcome derivation
#' @param serious Serious AE derivation.
#' @param serious_withdrawl Serious AE leading to withdrawal derivation
#' @param serious_modified Serious AE leading to dose modification derivation
#' @param serious_related Related Serious AE derivation
#' @param withdrawl AE leading to withdrawal derivation
#' @param modified AE leading to dose modification derivation
#' @param related Related AE derivation
#' @param related_withdrawl Related AE leading to withdrawal derivation
#' @param related_modified Related AE leading to dose modification derivation
#' @param ctc35 Grade 3-5 AE derivation
#' @param ... named expressions used to generate categories
#' @details in this function, all flags are expressions calls, for simpler usage.
#' @export
#' @examples
#' library(scda)
#' library(rtables)
#' library(dplyr)
#'
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' # add additional dummy causality flags
#' ADAE <- ADAE %>%
#'   mutate(AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X")) %>%
#'   mutate(AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo")) %>%
#'   rtables::var_relabel(
#'     AEREL1 = "AE related to A: Drug X",
#'     AEREL2 = "AE related to B: Placebo"
#'   )
#'
#' create_flag_vars(ADAE)
#' # create other flags
#' create_flag_vars(ADAE, `AENSER` = AESER != "Y")
#' # remove flags that are not needed
#' create_flag_vars(ADAE, fatal = NULL)
create_flag_vars <- function(df,
                             # nolint start
                             fatal = AESDTH == "Y",
                             serious = AESER == "Y",
                             serious_withdrawl = AESER == "Y" &
                               grepl("DRUG WITHDRAWN", AEACN),
                             serious_modified = AESER == "Y" &
                               grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
                             serious_related = AESER == "Y" & AEREL == "Y",
                             withdrawl = grepl("DRUG WITHDRAWN", AEACN),
                             modified = grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
                             related = AEREL == "Y",
                             related_withdrawl = AEREL == "Y" & grepl("DRUG WITHDRAWN", AEACN),
                             related_modified = AEREL == "Y" &
                               grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
                             ctc35 = AETOXGR %in% c("3", "4", "5"),
                             # nolint end
                             ...) {
  AESDTH <- AESER <- AEACN <- AEREL <- AETOXGR <- NULL # nolint
  args <-
    eval(substitute(
      alist(
        "AE with fatal outcome" = fatal,
        "Serious AE" = serious,
        "Serious AE leading to withdrawal" = serious_withdrawl,
        "Serious AE leading to dose modification" = serious_modified,
        "Related Serious AE" = serious_related,
        "AE leading to withdrawal" = withdrawl,
        "AE leading to dose modification" = modified,
        "Related AE" = related,
        "Related AE leading to withdrawal" = related_withdrawl,
        "Related AE leading to dose modification" = related_modified,
        "Grade 3-5 AE" = ctc35
      )
    ))
  args <- c(args, eval(substitute(alist(...))))
  stopifnot(all(names(args) != "")) # all elements in ... should be named
  argnames <- unique(names(args))
  df <- as.data.frame(df)
  ret <- lapply(argnames, function(t) {
    tryCatch(
      expr = {
        with(df, eval(args[[t]]))
      },
      error = function(w) {
        NULL
      },
      warning = function(w) {
        NULL
      }
    )
  })
  names(ret) <- argnames
  valid <- vapply(argnames, function(x) {
    valid <- length(ret[[x]]) > 0
    if (!valid) {
      warning(sprintf(
        "%s with calculation %s not valid",
        x,
        as.character(as.expression(args[[x]]))
      ))
    }
    valid
  }, FUN.VALUE = TRUE)
  do.call(data.frame, args = list(ret[valid], check.names = FALSE))
}
