#' Common Events Plot
#'
#' This function plots commonly occurred events by number of unique subjects with events.
#' It creates basic summary of events and compares event occurrences between treatment and reference
#' arms, and can be used for events data such as Adverse Events.
#'
#' @param term \code{character} or \code{factor} vector represents events information
#' @param id \code{vector} contains subject identifier. Length of \code{id} must be the same as
#'  the length or number of rows of \code{terms}. Ususally it is \code{USUBJID}.
#' @param arm \code{character} vector contains arm informatiion. For example, \code{ACTARMCD}.
#' @param arm_sl \code{character} vector contains subject level arm vector. For example, \code{ADSL$ACTARMCD}.
#' @param trt \code{character} indicates the name of the treatment arm
#' @param ref \code{character} indicates the name of the reference arm
#' @param sort_by \code{character} indicates sort_by type. Choose from "term", "riskdiff"
#' and "meanrisk". Default is "term".
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
#' @param draw whether to draw the Plot.
#' @details there is no equivalent STREAM output
#'
#' @return grob object
#'
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @importFrom data.table data.table dcast ":=" transpose
#' @importFrom grid textGrob unit unit.c grobHeight grobWidth
#' @importFrom DescTools BinomDiffCI
#' @importFrom utils.nest stop_if_not
#' @importFrom stats setNames
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
#' g_events_term_id(term, id, arm, arm_sl,
#'   sort_by = "meanrisk",
#'   reversed = FALSE, axis_side = "right"
#' )
#' g_events_term_id(term, id, arm, arm_sl, conf_level = 0.9, fontsize = 6)
#' term <- create_flag_vars(cadae)
#' g_events_term_id(term, id, arm, arm_sl, conf_level = 0.9, fontsize = 4)
g_events_term_id <- function(term,
                             id,
                             arm,
                             arm_sl,
                             trt = unique(arm)[1],
                             ref = unique(arm)[2],
                             sort_by = "term",
                             rate_range = c(0, 1),
                             diff_range = c(-1, 1),
                             reversed = FALSE,
                             conf_level = 0.95,
                             ci_method = "wald",
                             axis_side = "left",
                             color = c("red", "blue"),
                             shape = c(16, 17),
                             fontsize = 4,
                             draw = TRUE) {
  # check issues
  `.` <-
    `.N` <- #nolint
    total <-
    trt_count <-
    ref_count <-
    riskdiff <-
    meanrisk <- risk <- upper_ci <- lower_ci <- NULL #nolint
  if (is.data.frame(term)) {
    term_levels <- factor(colnames(term), levels = rev(colnames(term)))
    term <- data.table::transpose(term)
    term <- lapply(term, function(x) {
      term_levels[x]
    })
  }
  # argument validation
  possible_sort <- c("term", "riskdiff", "meanrisk")
  possible_axis <- c("left", "right")
  term <- force(term)
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
  df_n <- data.table(arm = arm_sl)[arm %in% arms,
                                   .(total = .N),
                                   by = .(arm)]
  trt_total <- df_n[arm == trt, total] #nolint
  ref_total <- df_n[arm == ref, total] #nolint
  df <-
    data.table(id, arm, term)[, list(term = as.character(unlist(term))), by = .(arm, id)]
  df <- unique(df)[arm %in% arms,
                   .N,
                   by = .(arm, term)]
  df[, arm := ifelse(arm == trt, "trt_count", "ref_count")]
  df <-
    dcast(
      df,
      term ~ arm,
      value.var = c("N"),
      drop = FALSE,
      fill = 0
    )

  df[, `:=`(trt_total = trt_total, ref_total = ref_total)]

  df_ci <- df[, .(
    value = c(
      BinomDiffCI(
        trt_count,
        trt_total,
        ref_count,
        ref_total,
        conf_level,
        method = ci_method
      )[1, ],
      (trt_count + ref_count) / (trt_total + ref_total)
    ),
    param = c("riskdiff", "lower_ci", "upper_ci", "meanrisk")
  ), by = .(term)]
  df_ci <- dcast(df_ci, term ~ param, variable.var = "value")
  df_risk <- df[, .(
    risk = c(trt_count / trt_total, ref_count / ref_total),
    arm = c(trt, ref)
  ), by = .(term)]
  names(color) <- arms
  names(shape) <- arms

  terms_needed <- unique(df_ci[(riskdiff > diff_range[1] &
                                  riskdiff < diff_range[2]) &
                                 (meanrisk > rate_range[1] &
                                    meanrisk < rate_range[2]),
                               term])

  if (length(terms_needed) == 0) {
    ret <- textGrob("All Observations are filtered out")
    if (draw) {
      grid.draw(ret)
    }
    return(invisible(ret))
  }
  if (sort_by == "term") {
    terms_needed <- sort(terms_needed, decreasing = TRUE)
  } else {
    terms_needed <-
      df_ci[order(df_ci[[sort_by]], decreasing = FALSE)][term %in% terms_needed, term]
  }

  if (reversed) {
    terms_needed <- rev(terms_needed)
  }

  df_ci <- df_ci[term %in% terms_needed]
  df_ci[, term := factor(term, terms_needed)]
  df_risk <- df_risk[term %in% terms_needed]
  df_risk[, term := factor(term, terms_needed)]
  terms_label <- vapply(
    lapply(terms_needed, strwrap, width = 30),
    paste,
    FUN.VALUE = character(1),
    collapse = "\n"
  )

  mytheme <-
    theme_osprey(axis_side = axis_side, fontsize = fontsize)

  labels <-
    setNames(df_n[, sprintf("%s\n(N = %i)", arm, total)], df_n$arm)

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

  p2 <- ggplot(df_ci) +
    geom_point(mapping = aes(y = term, x = riskdiff),
               size = fontsize * 0.7) +
    geom_vline(data = NULL,
               xintercept = 0,
               linetype = 2) +
    mytheme +
    geom_errorbarh(mapping = aes(xmax = upper_ci, xmin = lower_ci, y = term),
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
  } else {
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
  if (draw) {
    grid.draw(ret)
  }
  invisible(ret)
}


#' default ae overview flags
#' @importFrom data.table data.table as.data.table
#' @param df data frame of ae. use default
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
#' @param grade3 Grade 3-5 AE derivation
#' @param ... named expressions used to generate categories
#' @details in this function, all flags are expressions calls, for simpler usage.
#' @export
#' @examples
#' library(random.cdisc.data)
#' create_flag_vars(cadae)
#' create_flag_vars(cadae, `AENSER` = AESER != "Y") # create other flags
#' create_flag_vars(cadae, fatal = NULL) # remove not needed flags
create_flag_vars <- function(df,
                             fatal = AESDTH == "Y",
                             #nolint
                             serious = AESER == "Y",
                             #nolint
                             serious_withdrawl = AESER == "Y" &
                               grepl("DRUG WITHDRAWN", AEACN),
                             #nolint
                             serious_modified = AESER == "Y" &
                               grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
                             #nolint
                             serious_related = AESER == "Y" &
                               AEREL == "Y",
                             #nolint
                             withdrawl = grepl("DRUG WITHDRAWN", AEACN),
                             #nolint
                             modified = grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
                             #nolint
                             related = AEREL == "Y",
                             #nolint
                             related_withdrawl = AEREL == "Y" &
                               grepl("DRUG WITHDRAWN", AEACN),
                             #nolint
                             related_modified = AEREL == "Y" &
                               grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
                             #nolint
                             grade3 = AETOXGR %in% c("3", "4", "5"),
                             #nolint
                             ...) {
  AESDTH <- AESER <- AEACN <- AEREL <- AETOXGR <- NULL #nolint
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
        "Grade 3-5 AE" = grade3
      )
    ))
  args <- c(args, eval(substitute(alist(...))))
  stopifnot(all(names(args) != "")) # all elements in ... should be named
  argnames <- unique(names(args))
  df <- as.data.table(df)
  ret <- lapply(argnames, function(t) {
    tryCatch({
      df[, eval(args[[t]])]
    },
    error = function(w) {
      NULL
    },
    warning = function(w) {
      NULL
    })
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
  do.call(data.table, args = ret[valid])
}


#' allow data.table in pacakge
.datatable.aware <- TRUE #nolint
