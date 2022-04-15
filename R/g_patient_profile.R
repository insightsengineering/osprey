#' Patient Domain Profile
#'
#' Patient domain profile provides information for a specific subject that participated in the study.
#' The plot includes relevant data for one subject in a user specified domain, including
#' adverse events (\code{ADAE}), response (\code{ADRS}), concomitant medications
#' (\code{ADCM}), exposure (\code{ADEX}), and laboratory (\code{ADLB}).
#'
#' @param domain string of domain name to be shown as y-axis label, default is `NULL`
#' (no y-axis label shown)
#' @param var_names character vector to identify each lane
#' @param marker_pos Depending on the domain, this can be
#' \itemize{
#' \item marker position numeric vector for domains \code{ADEX}, \code{ADLB}, and \code{ADRS}
#' \item numeric data frame with two columns, start and end time marker position,
#' for domains \code{ADAE} and \code{ADCM}
#' }
#' @param arrow_end numeric value indicates the end of arrow when arrows are requested
#' @param xtick_at numeric vector with the locations of the x-axis tick marks
#' @param line_col_list a list may contain \cr
#' \itemize{
#' \item \code{line_col}: factor vector to specify color for segments , default is `NULL`
#' (no line color is specified)\cr
#' \item \code{line_col_opt} aesthetic values to map color values (named vector to map color values to each name).
#'      If not `NULL`, please make sure this contains all possible values for \code{line_col} values,
#'      otherwise color will be assigned by \code{\link[grDevices]{hcl.colors}}
#' \item \code{line_col_legend}: a string to be displayed as line color legend title when \code{line_col} is specified,
#'  default is `NULL` (no legend title is displayed)
#' }
#' @param line_width numeric value for segment width, default is \code{line_width = 1}
#' @param arrow_size numeric value for arrow size, default is \code{arrow_size = 0.1}
#' @param no_enddate_extention numeric value for extending the arrow when end date is missing for \code{ADAE}
#' or \code{ADCM} domain. Default is \code{no_enddate_extention = 0}.
#' @param marker_col_list a list may contain \cr
#' \itemize{
#' \item \code{marker_col} a factor vector to specify color for markers,
#' default is `NULL` (no color markers is specified)
#' \item \code{marker_col_opt} aesthetic values to map color values (named vector to map color values to each name)
#'      If not `NULL`, please make sure this contains all possible values for \code{marker_col} values,
#'      otherwise color will be assigned by \code{\link[grDevices]{hcl.colors}}
#' \item \code{marker_col_legend} a string to be displayed as marker color legend title, default is `NULL`
#' (no legend title is displayed)
#' }
#' @param marker_shape_list a list may contain \cr
#' \itemize{
#' \item \code{marker_shape} factor vector to specify shape for markers,
#' default is `NULL` (no shape marker is specified)
#' \item  \code{marker_shape_opt} aesthetic values to map shape values (named vector to map shape values to each name).
#'      If not `NULL`, please make sure this contains all possible values for \code{marker_shape} values,
#'      otherwise shape will be assigned by \code{ggplot} default
#' \item \code{marker_shape_legend} string to be displayed as marker shape legend title, default is `NULL`
#' (no legend title is displayed)
#' }
#' @param show_days_label boolean value for showing y-axis label, default is \code{TRUE}
#' @param xlim numeric vector for x-axis limit, default is
#'     \code{xlim = c(-28, max(marker_pos) + 5)}
#' @param xlab string to be shown as x-axis label, default is \code{"Study Day"}
#' @param show_title boolean value for showing title of the plot, default is \code{TRUE}
#' @param title string to be shown as title of the plot, default is `NULL` (no plot title is displayed)
#'
#' @author Xuefeng Hou (houx14) \email{houx14@gene.com}
#' @author Tina Cho (chot) \email{tina.cho@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#' @template author_qit3
#'
#' @return plot object
#'
#' @export
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' # ADSL
#' rADSL <- synthetic_cdisc_data("latest")$adsl
#' ADSL <- rADSL %>%
#'   group_by(USUBJID) %>%
#'   mutate(
#'     TRTSDT = as.Date(TRTSDTM),
#'     max_date = max(as.Date(LSTALVDT), as.Date(DTHDT), na.rm = TRUE),
#'     max_day = as.numeric(as.Date(max_date) - as.Date(TRTSDT)) + 1
#'   ) %>%
#'   select(USUBJID, STUDYID, TRTSDT, max_day) %>%
#'   filter(USUBJID == rADSL$USUBJID[1])
#'
#'
#' # Example 1 Exposure "ADEX"
#' rADEX <- synthetic_cdisc_data("latest")$adex
#' ADEX <- rADEX %>%
#'   select(USUBJID, STUDYID, ASTDTM, PARCAT2, AVAL, AVALU, PARAMCD)
#' ADEX <- left_join(ADSL, ADEX, by = c("USUBJID", "STUDYID"))
#' ADEX <- ADEX %>%
#'   filter(PARAMCD == "DOSE") %>%
#'   arrange(PARCAT2, PARAMCD) %>%
#'   mutate(diff = c(0, diff(AVAL, lag = 1))) %>%
#'   mutate(
#'     Modification = case_when(
#'       diff < 0 ~ "Decrease",
#'       diff > 0 ~ "Increase",
#'       diff == 0 ~ "None"
#'     )
#'   ) %>%
#'   mutate(
#'     ASTDT_dur = as.numeric(
#'       as.Date(
#'         substr(as.character(ASTDTM), 1, 10)
#'       ) - as.Date(TRTSDT) + 1
#'     )
#'   )
#'
#' p1 <- patient_domain_profile(
#'   domain = "Exposure (ADEX)",
#'   var_names = ADEX$PARCAT2,
#'   marker_pos = ADEX$ASTDT_dur,
#'   arrow_end = ADSL$max_day,
#'   xtick_at = waiver(),
#'   line_col_list = NULL,
#'   line_width = 1,
#'   arrow_size = 0.1,
#'   no_enddate_extention = 0,
#'   marker_col_list = list(
#'     marker_col = factor(ADEX$Modification),
#'     marker_col_opt = c("Increase" = "red", "Decrease" = "green", "None" = "blue"),
#'     marker_col_legend = NULL
#'   ),
#'   marker_shape_list = list(
#'     marker_shape = factor(ADEX$Modification),
#'     marker_shape_opt = c("Increase" = 24, "Decrease" = 25, "None" = 23),
#'     marker_shape_legend = "Dose Modification"
#'   ),
#'   show_days_label = TRUE,
#'   xlim = c(-28, ADSL$max_day),
#'   xlab = "Study Day",
#'   title = paste("Patient Profile: ", ADSL$USUBJID)
#' )
#' p1
#'
#' # Example 2 Adverse Event "ADAE"
#' # Note that ASTDY is represented by a circle and AENDY is represented by a square.
#' # If AENDY and ASTDY occur on the same day only AENDY will be shown.
#'
#' # Adverse Event ADAE
#' rADAE <- synthetic_cdisc_data("latest")$adae
#' ADAE <- rADAE %>%
#'   select(USUBJID, STUDYID, AESOC, AEDECOD, AESER, AETOXGR, AEREL, ASTDY, AENDY)
#' ADAE <- left_join(ADSL, ADAE, by = c("USUBJID", "STUDYID"))
#'
#' p2 <- patient_domain_profile(
#'   domain = "Adverse Event (ADAE)",
#'   var_names = ADAE$AEDECOD,
#'   marker_pos = ADAE[, c("ASTDY", "AENDY")],
#'   arrow_end = ADSL$max_day,
#'   xtick_at = waiver(),
#'   line_col_list = list(
#'     line_col = ADAE$AESER,
#'     line_col_legend = "Serious",
#'     line_col_opt = c("blue", "green")
#'   ),
#'   line_width = 1,
#'   arrow_size = 0.1,
#'   no_enddate_extention = 0,
#'   marker_col_list = list(
#'     marker_col = factor(ADAE$AETOXGR),
#'     marker_col_opt = c("3" = "yellow", "4" = "red"),
#'     marker_col_legend = NULL
#'   ),
#'   marker_shape_list = list(
#'     marker_shape = NULL,
#'     marker_shape_opt = NULL,
#'     marker_shape_legend = "Grade"
#'   ),
#'   show_days_label = TRUE,
#'   xlim = c(-28, ADSL$max_day),
#'   xlab = "Study Day",
#'   title = paste("Patient Profile: ", ADSL$USUBJID)
#' )
#' p2
#'
#' # Example 3 Tumor Response "ADRS"
#' rADRS <- synthetic_cdisc_data("latest")$adrs
#' ADRS <- rADRS %>%
#'   select(USUBJID, STUDYID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADTM)
#' ADRS <- left_join(ADSL, ADRS, by = c("USUBJID", "STUDYID"))
#' p3 <- patient_domain_profile(
#'   domain = "Tumor Response (ADRS)",
#'   var_names = ADRS$PARAMCD,
#'   marker_pos = ADRS$ADY,
#'   arrow_end = ADSL$max_day,
#'   xtick_at = waiver(),
#'   line_col_list = NULL,
#'   line_width = 1,
#'   arrow_size = 0.1,
#'   no_enddate_extention = 0,
#'   marker_col_list = list(
#'     marker_col = factor(ADRS$AVALC),
#'     marker_col_opt = c(
#'       "CR" = "green", "PR" = "blue",
#'       "SD" = "yellow", "PD" = "red", "NE" = "pink",
#'       "Y" = "lightblue", "N" = "darkred"
#'     ),
#'     marker_col_legend = NULL
#'   ),
#'   marker_shape_list = list(
#'     marker_shape = factor(ADRS$AVALC),
#'     marker_shape_opt = c(
#'       "CR" = 21, "PR" = 24,
#'       "SD" = 23, "PD" = 22, "NE" = 14,
#'       "Y" = 11, "N" = 8
#'     ),
#'     marker_shape_legend = "Response"
#'   ),
#'   show_days_label = TRUE,
#'   xlim = c(-28, ADSL$max_day),
#'   xlab = "Study Day",
#'   title = paste("Patient Profile: ", ADSL$USUBJID)
#' )
#' p3
#'
#' # Example 4 Concomitant Med "ADCM"
#' rADCM <- synthetic_cdisc_data("latest")$adcm
#' ADCM <- rADCM %>%
#'   select(USUBJID, STUDYID, ASTDTM, AENDTM, CMDECOD, ASTDY, AENDY)
#' ADCM <- left_join(ADSL, ADCM, by = c("USUBJID", "STUDYID"))
#' p4 <- patient_domain_profile(
#'   domain = "Concomitant Med (ADCM)",
#'   var_names = ADCM$CMDECOD,
#'   marker_pos = ADCM[, c("ASTDY", "AENDY")],
#'   arrow_end = ADSL$max_day,
#'   xtick_at = waiver(),
#'   line_col_list = list(line_col_opt = "orange"),
#'   line_width = 1,
#'   arrow_size = 0.1,
#'   no_enddate_extention = 50,
#'   marker_col_list = list(marker_col_opt = "orange"),
#'   marker_shape_list = NULL,
#'   show_days_label = TRUE,
#'   xlim = c(-28, ADSL$max_day),
#'   xlab = "Study Day",
#'   title = paste("Patient Profile: ", ADSL$USUBJID)
#' )
#' p4
#'
#' # Example 5 Laboratory "ADLB"
#' rADLB <- synthetic_cdisc_data("latest")$adlb
#' ADLB <- rADLB %>%
#'   select(
#'     USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE,
#'     ADTM, ADY, ATPTN, AVISITN, LBTESTCD, ANRIND
#'   )
#' ADLB <- left_join(ADSL, ADLB, by = c("USUBJID", "STUDYID"))
#'
#' ADLB <- ADLB %>%
#'   group_by(USUBJID) %>%
#'   mutate(ANRIND = factor(ANRIND, levels = c("LOW", "NORMAL", "HIGH")))
#'
#' p5 <- patient_domain_profile(
#'   domain = "Laboratory (ADLB)",
#'   var_names = ADLB$LBTESTCD,
#'   marker_pos = ADLB$ADY,
#'   arrow_end = ADSL$max_day,
#'   xtick_at = waiver(),
#'   line_col_list = NULL,
#'   line_width = 1,
#'   arrow_size = 0.1,
#'   no_enddate_extention = 0,
#'   marker_col_list = list(
#'     marker_col = factor(ADLB$ANRIND),
#'     marker_col_opt = c(
#'       "HIGH" = "red", "LOW" = "blue",
#'       "NORMAL" = "green", "NA" = "green"
#'     )
#'   ),
#'   marker_shape_list = list(
#'     marker_shape = factor(ADLB$ANRIND),
#'     marker_shape_opt = c(
#'       "HIGH" = 24, "LOW" = 25,
#'       "NORMAL" = 23, "NA" = 23
#'     ),
#'     marker_shape_legend = "Labs Abnormality"
#'   ),
#'   show_days_label = TRUE,
#'   xlim = c(-30, ADSL$max_day),
#'   xlab = "Study Day",
#'   title = paste("Patient Profile: ", ADSL$USUBJID)
#' )
#' p5
patient_domain_profile <- function(domain = NULL,
                                   var_names,
                                   marker_pos,
                                   arrow_end,
                                   xtick_at = waiver(),
                                   line_col_list = NULL,
                                   line_width = 1,
                                   arrow_size = 0.1,
                                   no_enddate_extention = 0,
                                   marker_col_list = NULL,
                                   marker_shape_list = NULL,
                                   show_days_label = TRUE,
                                   xlim = c(-28, max(marker_pos) + 5),
                                   xlab = NULL,
                                   show_title = TRUE,
                                   title = NULL) {
  line_col <- line_col_list[["line_col"]]
  line_col_opt <- line_col_list[["line_col_opt"]]
  line_col_legend <- line_col_list[["line_col_legend"]]

  marker_col <- marker_col_list[["marker_col"]]
  marker_col_opt <- marker_col_list[["marker_col_opt"]]
  marker_col_legend <- marker_col_list[["marker_col_legend"]]

  marker_shape <- marker_shape_list[["marker_shape"]]
  marker_shape_opt <- marker_shape_list[["marker_shape_opt"]]
  marker_shape_legend <- marker_shape_list[["marker_shape_legend"]]

  # check user input
  stopifnot(
    "invalid arguments: check that the length of input arguments are identical" =
      length(unique(nrow(data.frame(var_names)), nrow(data.frame(marker_pos)), nrow(data.frame(arrow_end)))) == 1
  )
  stopifnot(
    "invalid argument: check that marker_pos is either a vector or a data frame with two columns" =
      ncol(data.frame(marker_pos)) <= 2
  )
  stopifnot(
    "invalid arguments: check that the length of line_col is equal as other inputs" =
      is.null(line_col) || length(line_col) == length(var_names)
  )
  stopifnot(
    "invalid arguments: check that the length of marker_col is equal as other inputs" =
      is.null(marker_col) || length(marker_col) == length(var_names)
  )
  stopifnot(
    "invalid arguments: check that the length of marker_shape is equal as other inputs" =
      is.null(marker_shape) || length(marker_shape) == length(var_names)
  )
  checkmate::assert_numeric(xlim, len = 2)

  marker_data <- data.frame(
    var_names,
    marker_pos = if (is.null(marker_pos)) to_n("x", length(var_names)) else marker_pos,
    marker_shape = if (is.null(marker_shape)) to_n("x", length(var_names)) else marker_shape,
    marker_col = if (is.null(marker_col)) to_n("x", length(var_names)) else marker_col
  )

  # plot lines
  if (length(dim(marker_pos)) == 2) {
    line_data <- data.frame(
      var_names,
      line_col = if (is.null(line_col)) to_n("x", length(var_names)) else line_col,
      line_start = unname(marker_pos[, 1]),
      line_end = unname(marker_pos[, 2]),
      line_min = rep(xlim[1], length(var_names)),
      line_max = rep(arrow_end + no_enddate_extention, length(var_names))
    )
    names(line_data) <- c("var_names", "line_col", "line_start", "line_end", "line_min", "line_max")

    p <- ggplot() +
      geom_segment(
        data = line_data[!is.na(line_data$line_end), ],
        aes(
          x = var_names,
          y = line_start,
          xend = var_names,
          yend = line_end,
          color = line_col
        ),
        lineend = "round", linejoin = "round",
        size = line_width, arrow = NULL, show.legend = NA,
        na.rm = TRUE
      ) +
      scale_y_continuous(limits = xlim, breaks = xtick_at, expand = c(0, 0)) +
      coord_flip(xlim = c(1, length(unique(var_names)))) +
      geom_segment(
        data = line_data[is.na(line_data$line_end) == TRUE, ],
        aes(
          x = var_names,
          y = pmax(line_start, line_min, na.rm = TRUE),
          xend = var_names,
          yend = line_max,
          color = line_col
        ),
        lineend = "round", linejoin = "round",
        size = line_width, show.legend = FALSE,
        arrow = arrow(length = grid::unit(arrow_size, "inches")),
        na.rm = TRUE
      )

    if (is.null(line_col_opt)) line_col_opt <- grDevices::hcl.colors(length(levels(line_data$line_col)))

    p <- p +
      scale_color_manual(
        breaks = line_data$line_col,
        values = line_col_opt,
        limits = levels(line_data$line_col)
      )

    if (!is.null(line_col)) {
      p <- p + guides(color = guide_legend(line_col_legend, order = 1))
    } else {
      p <- p + guides(color = FALSE)
    }

    # plot markers
    p <- p +
      geom_point(
        data = marker_data,
        aes(x = var_names, y = marker_data[, 2], fill = factor(marker_col)),
        shape = 21,
        size = 5,
        na.rm = TRUE
      ) +
      geom_point(
        data = marker_data,
        aes(x = var_names, y = marker_data[, 3], fill = factor(marker_col)),
        shape = 22,
        size = 3,
        na.rm = TRUE
      )

    if (is.null(marker_col_opt)) marker_col_opt <- grDevices::hcl.colors(length(levels(marker_data$marker_col)))
    p <- p +
      scale_fill_manual(
        breaks = marker_data$marker_col,
        values = marker_col_opt
      )


    p <- p + theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      ylab(xlab) + xlab(domain)

    if (!is.null(marker_col)) {
      p <- p + guides(fill = guide_legend(marker_col_legend, order = 2))
    } else {
      p <- p + guides(fill = FALSE)
    }

    p <- p + guides(shape = guide_legend("Shape", order = 3))

    p <- p + scale_shape_manual(values = c(21, 22)) +
      guides(shape = guide_legend(title = "Shape", override.aes = list(label = c("Start", "End")), order = 3))

    if (!is.null(marker_shape)) {
      p <- p + guides(shape = guide_legend(marker_shape_legend, order = 3))
    } else {
      p <- p + guides(shape = FALSE)
    }
  } else {
    p <- ggplot() +
      geom_point(
        data = marker_data,
        aes(
          x = var_names,
          y = marker_pos,
          shape = marker_shape,
          fill = marker_col
        ),
        size = 3, na.rm = TRUE
      ) +
      scale_y_continuous(limits = xlim, breaks = xtick_at, expand = c(0, 0)) +
      coord_flip(xlim = c(1, length(unique(var_names)))) +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      ylab(xlab) +
      xlab(domain)

    if (is.null(marker_col_legend)) {
      if (length(setdiff(marker_col, marker_shape)) == 0) {
        marker_col_legend <- marker_shape_legend
      }
    }

    if (is.null(marker_shape_legend)) {
      if (length(setdiff(marker_col, marker_shape)) == 0) {
        marker_shape_legend <- marker_col_legend
      }
    }

    if (is.null(marker_col_opt)) marker_col_opt <- grDevices::hcl.colors(length(levels(marker_data$marker_col)))
    p <- p +
      scale_fill_manual(
        name = marker_col_legend,
        breaks = marker_data$marker_col,
        values = marker_col_opt
      )

    if (is.null(marker_shape_opt)) marker_shape_opt <- 1:25
    p <- p + scale_shape_manual(
      name = marker_shape_legend,
      breaks = marker_data$marker_shape,
      values = marker_shape_opt
    )
  }

  # plot title and labels
  if (show_title) {
    p <- p +
      labs(title = title) +
      theme(plot.title = element_text(face = "bold"))
  }

  # Plot y axis label
  if (show_days_label == FALSE) {
    p <- p + theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)),
        legend.title = element_text(size = 10),
        legend.spacing.y = grid::unit(0, "cm"),
        legend.key.height = grid::unit(1, "line"),
        legend.margin = margin(t = 0, b = 0, r = 0.5, l = 0, unit = "cm"),
        plot.margin = margin(t = 0, b = 0, r = 0.5, l = 0.5, unit = "cm")
      )
  } else {
    p <- p + theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_text(size = 10),
        legend.spacing.y = grid::unit(0, "cm"),
        legend.key.height = grid::unit(1, "line"),
        legend.margin = margin(t = 0, b = 0, r = 0.5, l = 0, unit = "cm"),
        plot.margin = margin(t = 0, b = 0, l = 0.5, r = 0.5, unit = "cm")
      )
  }

  p
}

#' Patient Profile Plot
#'
#' Patient profile plot provides detailed information for a specific subject participating in the study.
#' The plot includes relevant data for one subject that can help correlate adverse events, response,
#' concomitant medications, exposure, and laboratory. The plotting of patient profile is modularized, with
#' each domain plot generated by function \code{\link{patient_domain_profile}}. This \code{\link{g_patient_profile}}
#' function assembles all requested domain plots into one patient profile.
#' \code{ADSL}, \code{ADEX}, \code{ADAE}, \code{ADRS}, \code{ADCM} and \code{ADLB} data must be provided.
#' The plot output will not include domains with data unspecified
#'
#' @param ex list may contain
#' \itemize{
#' \item \code{data} dataframe for \code{ADEX} domain dataset
#' \item \code{var} vector to identify each lane of \code{ADEX} domain plot
#' }
#' @param ae list may contain
#' \itemize{
#' \item \code{data} dataframe for \code{ADAE} domain dataset
#' \item \code{var} vector to identify each lane of \code{ADAE} plot
#' \item \code{line_col} factor vector to specify color for segments of \code{ADAE} plot
#' \item \code{line_col_legend} string to be displayed as line color legend title of \code{ADAE} plot
#' \item \code{line_col_opt} aesthetic values to map line color values of \code{ADAE} plot
#'      (named vector to map color values to each name).
#'      If not `NULL`, please make sure this contains all possible values for \code{line_col} values,
#'      otherwise color will be assigned by \code{ggplot} default, please note that `NULL` needs to be
#'      specified
#' }
#' @param rs list may contain
#' \itemize{
#' \item \code{data} dataframe for \code{ADRS} domain dataset
#' \item \code{var} vector to identify each lane of \code{ADRS} domain plot
#' }
#' @param cm list may contain
#' \itemize{
#' \item \code{data} dataframe for \code{ADCM} domain dataset
#' \item \code{var} vector to identify each lane of \code{ADCM} domain plot
#' }
#' @param lb list may contain
#' \itemize{
#' \item \code{data} dataframe for \code{ADLB} domain dataset
#' \item \code{var} vector to identify each lane of \code{ADLB} domain plot
#' }
#' @param arrow_end_day numeric value indicates the end of arrow when arrows are requested
#' @param xlim numeric vector for x-axis limit that will be shared by all domain plots, default is
#'      \code{xlim = c(-28, 365)}
#' @param xlab string to be shown as x-axis label, default is \code{"Study Day"}
#' @param title string to be shown as title of the plot, default is \code{"Patient Profile"}
#'
#' @author Xuefeng Hou (houx14) \email{houx14@gene.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#' @template author_qit3
#'
#' @return plot object
#'
#' @export
#'
#' @seealso \code{\link{patient_domain_profile}}
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' # ADSL
#' rADSL <- synthetic_cdisc_data("latest")$adsl
#' ADSL <- rADSL %>%
#'   group_by(USUBJID) %>%
#'   mutate(
#'     TRTSDT = as.Date(TRTSDTM),
#'     max_date = max(as.Date(LSTALVDT), as.Date(DTHDT), na.rm = TRUE),
#'     max_day = as.numeric(as.Date(max_date) - as.Date(TRTSDT)) + 1
#'   ) %>%
#'   select(USUBJID, STUDYID, TRTSDT, max_day) %>%
#'   filter(USUBJID == rADSL$USUBJID[1])
#'
#' # ADEX
#' rADEX <- synthetic_cdisc_data("latest")$adex
#' ADEX <- rADEX %>%
#'   select(USUBJID, STUDYID, ASTDTM, PARCAT2, AVAL, AVALU, PARAMCD)
#' ADEX <- left_join(ADSL, ADEX, by = c("USUBJID", "STUDYID"))
#'
#' ADEX <- ADEX %>%
#'   filter(PARAMCD == "DOSE") %>%
#'   arrange(PARCAT2, PARAMCD) %>%
#'   mutate(diff = c(0, diff(AVAL, lag = 1))) %>%
#'   mutate(Modification = case_when(
#'     diff < 0 ~ "Decrease",
#'     diff > 0 ~ "Increase",
#'     diff == 0 ~ "None"
#'   )) %>%
#'   mutate(ASTDT_dur = as.numeric(
#'     as.Date(substr(as.character(ASTDTM), 1, 10)) -
#'       as.Date(TRTSDT) + 1
#'   ))
#'
#' # ADAE
#' rADAE <- synthetic_cdisc_data("latest")$adae
#' ADAE <- rADAE %>%
#'   select(USUBJID, STUDYID, AESOC, AEDECOD, AESER, AETOXGR, AEREL, ASTDY, AENDY)
#' ADAE <- left_join(ADSL, ADAE, by = c("USUBJID", "STUDYID"))
#'
#' # ADRS
#' rADRS <- synthetic_cdisc_data("latest")$adrs
#' ADRS <- rADRS %>%
#'   select(USUBJID, STUDYID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADTM)
#' ADRS <- left_join(ADSL, ADRS, by = c("USUBJID", "STUDYID"))
#'
#' # ADCM
#' rADCM <- synthetic_cdisc_data("latest")$adcm
#' ADCM <- rADCM %>%
#'   select(USUBJID, STUDYID, ASTDTM, AENDTM, CMDECOD, ASTDY, AENDY)
#' ADCM <- left_join(ADSL, ADCM, by = c("USUBJID", "STUDYID"))
#'
#' # ADLB
#' rADLB <- synthetic_cdisc_data("latest")$adlb
#' ADLB <- rADLB %>%
#'   select(
#'     USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE, ADTM,
#'     ADY, ATPTN, AVISITN, LBTESTCD, ANRIND
#'   )
#' ADLB <- left_join(ADSL, ADLB, by = c("USUBJID", "STUDYID"))
#'
#' ADLB <- ADLB %>%
#'   group_by(USUBJID) %>%
#'   mutate(ANRIND = factor(ANRIND, levels = c("LOW", "NORMAL", "HIGH")))
#'
#' # Example Patient Profile plot 5 domains
#' g_patient_profile(
#'   ex = list(
#'     data = ADEX,
#'     var = ADEX$PARCAT2
#'   ),
#'   ae = list(
#'     data = ADAE,
#'     var = ADAE$AEDECOD,
#'     line_col = factor(ADAE$AESER),
#'     line_col_legend = "Serious",
#'     line_col_opt = c("Y" = "red", "N" = "blue")
#'   ),
#'   rs = list(
#'     data = ADRS,
#'     var = ADRS$PARAMCD
#'   ),
#'   cm = list(
#'     data = ADCM,
#'     var = ADCM$CMDECOD
#'   ),
#'   lb = list(
#'     data = ADLB,
#'     var = ADLB$LBTESTCD
#'   ),
#'   arrow_end_day = ADSL$max_day,
#'   xlim = c(-28, ADSL$max_day),
#'   xlab = "Study Day",
#'   title = paste("Patient Profile: ", ADSL$USUBJID)
#' )
#'
#' # Example Patient Profile plot without ADCM and ADLB
#' g_patient_profile(
#'   ex = list(
#'     data = ADEX,
#'     var = ADEX$PARCAT2
#'   ),
#'   ae = list(
#'     data = ADAE,
#'     var = ADAE$AEDECOD,
#'     line_col = factor(ADAE$AESER),
#'     line_col_legend = "Serious",
#'     line_col_opt = c("Y" = "red", "N" = "blue")
#'   ),
#'   rs = list(
#'     data = ADRS,
#'     var = ADRS$PARAMCD
#'   ),
#'   arrow_end_day = ADSL$max_day,
#'   xlim = c(-28, ADSL$max_day),
#'   xlab = "Study Day",
#'   title = paste("Patient Profile: ", ADSL$USUBJID)
#' )
g_patient_profile <- function(ex = NULL,
                              ae = NULL,
                              rs = NULL,
                              cm = NULL,
                              lb = NULL,
                              arrow_end_day,
                              xlim = c(-28, 365),
                              xlab = "Study Day",
                              title = "Patient Profile") {
  domains <- list(ex = ex, ae = ae, rs = rs, cm = cm, lb = lb)
  select <- mapply(domain_check, domains, names(domains))
  names(select) <- names(domains)

  show_days_label <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  show_days_label[max(which(select == TRUE))] <- TRUE

  show_title <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  show_title[min(which(select == TRUE))] <- TRUE

  # Domain "ADEX"
  if (select["ex"]) {
    p1 <- patient_domain_profile(
      domain = "Exposure (ADEX)",
      var_names = ex$var,
      marker_pos = ex$data$ASTDT_dur,
      arrow_end = arrow_end_day,
      xtick_at = waiver(),
      line_col_list = NULL,
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0,
      marker_col_list = list(
        marker_col = factor(ex$data$Modification),
        marker_col_opt = c("Increase" = "red", "Decrease" = "green", "None" = "blue")
      ),
      marker_shape_list = list(
        marker_shape = factor(ex$data$Modification),
        marker_shape_opt = c("Increase" = 24, "Decrease" = 25, "None" = 23),
        marker_shape_legend = "Dose Modification"
      ),
      show_days_label = show_days_label[1],
      xlim = xlim,
      xlab = xlab,
      show_title = show_title[1],
      title = title
    )
  } else {
    p1 <- NULL
  }
  # Domain "ADAE"
  if (select["ae"]) {
    p2 <- patient_domain_profile(
      domain = "Adverse Event (ADAE)",
      var_names = ae$var,
      marker_pos = ae$data[, c("ASTDY", "AENDY")],
      arrow_end = arrow_end_day,
      xtick_at = waiver(),
      line_col_list = list(
        line_col = ae$line_col,
        line_col_legend = ae$line_col_legend,
        line_col_opt = ae$line_col_opt
      ),
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0.1,
      marker_col_list = list(
        marker_col = factor(ae$data$AETOXGR),
        marker_col_opt = c("1" = "green", "2" = "blue", "3" = "yellow", "4" = "orange", "5" = "red"),
        marker_col_legend = "Grade"
      ),
      marker_shape_list = NULL,
      show_days_label = show_days_label[2],
      xlim = xlim,
      xlab = xlab,
      show_title = show_title[2],
      title = title
    )
  } else {
    p2 <- NULL
  }


  # Domain "ADRS"
  if (select["rs"]) {
    p3 <- patient_domain_profile(
      domain = "Response (ADRS)",
      var_names = rs$var,
      marker_pos = rs$data$ADY,
      arrow_end = arrow_end_day,
      xtick_at = waiver(),
      line_col_list = NULL,
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0,
      marker_col_list = list(
        marker_col = factor(rs$data$AVALC),
        marker_col_opt =
          c(
            "CR" = "green", "PR" = "blue", "SD" = "yellow", "PD" = "red",
            "NE" = "pink", "Y" = "lightblue", "N" = "darkred"
          )
      ),
      marker_shape_list = list(
        marker_shape = factor(rs$data$AVALC),
        marker_shape_opt = c("CR" = 21, "PR" = 24, "SD" = 23, "PD" = 22, "NE" = 14, "Y" = 11, "N" = 8),
        marker_shape_legend = "Response"
      ),
      show_days_label = show_days_label[3],
      xlim = xlim,
      xlab = xlab,
      show_title = show_title[3],
      title = title
    )
  } else {
    p3 <- NULL
  }


  # Domain "ADCM"
  if (select["cm"]) {
    p4 <- patient_domain_profile(
      domain = "Conmed (ADCM)",
      var_names = cm$var,
      marker_pos = cm$data[, c("ASTDY", "AENDY")],
      arrow_end = arrow_end_day,
      xtick_at = waiver(),
      line_col_list = list(line_col_opt = "orange"),
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0.1,
      marker_col_list = list(marker_col_opt = "orange"),
      marker_shape_list = NULL,
      show_days_label = show_days_label[4],
      xlim = xlim,
      xlab = xlab,
      show_title = show_title[4],
      title = title
    )
  } else {
    p4 <- NULL
  }

  # Domain "ADLB"
  if (select["lb"]) {
    p5 <- patient_domain_profile(
      domain = "Laboratory (ADLB)",
      var_names = lb$var,
      marker_pos = lb$data$ADY,
      arrow_end = arrow_end_day,
      xtick_at = waiver(),
      line_col_list = NULL,
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0,
      marker_col_list = list(
        marker_col = factor(lb$data$ANRIND),
        marker_col_opt = c("HIGH" = "red", "LOW" = "blue", "NORMAL" = "green")
      ),
      marker_shape_list = list(
        marker_shape = factor(lb$data$ANRIND),
        marker_shape_opt = c("HIGH" = 24, "LOW" = 25, "NORMAL" = 23),
        marker_shape_legend = "Labs Abnormality"
      ),
      show_days_label = show_days_label[5],
      xlim = xlim,
      xlab = xlab,
      show_title = show_title[5],
      title = title
    )
  } else {
    p5 <- NULL
  }

  # Assemble domain plots into patient profile plot
  plot_list <- list(p1, p2, p3, p4, p5)

  plot_list <- plot_list[select]
  # distribute space by number of levels in each domain
  var_list <- list(ex$var, ae$var, rs$var, cm$var, lb$var)
  var_list <- var_list %>%
    lapply(unique) %>%
    lapply(length) %>%
    unlist() %>%
    cbind(.data, select) %>%
    as.data.frame() %>%
    # keep the selected domains
    dplyr::filter(select == TRUE) %>%
    dplyr::mutate(nline_dat = ifelse(. <= 10 & . > 0, 10, .)) %>%
    # relative height
    dplyr::mutate(sbplt_ht = unlist(nline_dat) / sum(unlist(nline_dat)))

  cowplot::plot_grid(
    plotlist = plot_list,
    nrow = nrow(var_list),
    align = "v",
    axis = "lr",
    rel_heights = var_list$sbplt_ht
  )
}

#' a helper function for g_patient_profile to check whether the domain has data available
#' @param domain domain input from g_patient_profile
#' @param name domain names
#' @keywords internal
domain_check <- function(domain, name) {
  if (is.null(domain)) {
    select <- FALSE
  } else {
    select <- TRUE
    if (dim(domain$data)[1] == 0 || is.null(domain$data)) {
      warning(paste("No", name, "data for this subject"))
      select <- FALSE
    }
  }
  return(select)
}
