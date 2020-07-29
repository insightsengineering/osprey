#' Patient Domain Profile
#'
#' Patient domain profile provides information for a specific subject that participated in the study.
#' The plot includes relevant data for one subject in a user specified domain, including
#' adverse events (\code{ADAE}), response (\code{ADRS}), concomitant medications
#' (\code{ADCM}), exposure (\code{ADEX}), and laboratory (\code{ADLB}).
#'
#' @param domain string of domain name to be shown as y-axis label, default is \code{NULL}
#' (no y-axis label shown)
#' @param var_names character vector to identify each lane
#' @param marker_pos marker position, which needs to be a numeric vector for \code{ADEX}, \code{ADLB}
#' or \code{ADRS} for duration marker position, or a numeric data frame with two columns for
#' \code{ADAE} or \code{ADCM} for start and end time marker position
#' @param arrow_end numeric value indicates the end of arrow when arrows are requested
#' @param xtick_at optional break interval of bar length axis
#' @param line_col factor vector to specify color for segments, default is \code{NULL}
#' (no line color is specified)
#' @param line_col_legend string to be displayed as line color legend title when \code{line_col} is specified,
#'  default is \code{NULL} (no legend title is displayed)
#' @param line_col_opt aesthetic values to map color values (named vector to map color values to each name).
#'      If not \code{NULL}, please make sure this contains all posible values for \code{line_col} values,
#'      otherwise color will be assigned by \code{\link{ggplot}} default, please note that \code{NULL}
#'      needs to be specified
#' @param line_width numeric value for segment width, default is \code{line_width = 1}
#' @param arrow_size numeric value for arrow size, default is \code{arrow_size = 0.1}
#' @param no_enddate_extention numeric value for extending the arrow when end date is missing for \code{ADAE}
#' or \code{ADCM} domain. Default is \code{no_enddate_extention = 0}.
#' @param marker_color factor vector to specify color for markers, default is \code{NULL}
#' (no color markers is specified)
#' @param marker_color_opt aesthetic values to map color values (named vector to map color values to each name)
#'      If not \code{NULL}, please make sure this contains all posible values for \code{marker_color} values,
#'      otherwise color will be assigned by \code{ggplot} default, please note that \code{NULL} needs to be specified
#' @param marker_color_legend string to be displayed as marker color legend title, default is \code{NULL}
#' @param marker_shape factor vector to specify shape for markers, default is \code{NULL}
#' @param marker_shape_opt aesthetic values to map shape values (named vector to map shape values to each name).
#'      If not \code{NULL}, please make sure this contains all posible values for \code{marker_shape} values,
#'      otherwise shape will be assigned by \code{ggplot} default, please note that \code{NULL} needs to be specified
#' @param marker_shape_legend string to be displayed as marker shape legend title, default is \code{NULL}
#' @param show_days_label boolean value for showing y-axis label, default is \code{TRUE}
#' @param xlim numeric vector for x-axis limit, default is
#'     \code{xlim = c(-28, max(marker_pos) + 5)}
#' @param xlab string to be shown as x-axis label, default is \code{"Study Day"}
#' @param show_title boolean value for showing title of the plot, default is \code{TRUE}
#' @param title string to be shown as title of the plot, default is \code{NULL} (no plot title is depalyed)
#'
#' @author Xuefeng Hou (houx14) \email{houx14@gene.com}
#' @author Tina Cho (chot) \email{tina.cho@roche.com}
#' @author Molly He (hey59) \email{hey59@gene.com}
#' @template author_qit3
#'
#' @return plot object
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(utils.nest)
#' library(tern)
#' library(dplyr)
#'
#' # ADSL
#' rADSL <- radsl(cached = TRUE)
#' ADSL <-  rADSL %>%
#'   group_by(USUBJID) %>%
#'   mutate(TRTSDT = as.Date(TRTSDTM),
#'          max_date = max(as.Date(LSTALVDT), as.Date(DTHDT)),
#'          max_day = as.numeric(as.Date(max_date) - as.Date(TRTSDT)) + 1) %>%
#'   select(USUBJID, STUDYID, TRTSDT , max_day) %>%
#'   filter(USUBJID == rADSL$USUBJID[1])
#'
#' # Exposure ADEX
#' rADEX <- radex(cached = TRUE)
#' ADEX <- rADEX %>%
#'         select(USUBJID, STUDYID, ASTDTM, PARCAT2, AVAL, AVALU, PARAMCD)
#' ADEX <- left_join(ADSL, ADEX, by = c("USUBJID", "STUDYID"))
#'
#' ADEX <- ADEX %>%
#'   filter(PARAMCD == "DOSE") %>%
#'   arrange(PARCAT2, PARAMCD) %>%
#'   mutate(diff = c(0, diff(AVAL, lag = 1))) %>%
#'   mutate(Modification = case_when(diff < 0 ~ "Decrease",
#'                                   diff > 0 ~ "Increase",
#'                                   diff == 0 ~ "None")) %>%
#'   mutate(ASTDT_dur = as.numeric(as.Date(substr(as.character(ASTDTM), 1, 10)) -
#'                                 as.Date(TRTSDT) + 1))
#'
#' # Adverse Event ADAE
#' rADAE <- radae(cached = TRUE)
#' ADAE <- rADAE %>%
#'         select(USUBJID, STUDYID, AESOC, AEDECOD, AESER, AETOXGR, AEREL, ASTDY, AENDY)
#' ADAE <- left_join(ADSL, ADAE, by = c("USUBJID", "STUDYID"))
#'
#' # Tumor Response ADRS
#' rADRS <- radrs(cached = TRUE)
#' ADRS <- rADRS %>%
#'         select(USUBJID, STUDYID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADTM)
#' ADRS <- left_join(ADSL, ADRS, by = c("USUBJID", "STUDYID"))
#'
#' # Concomitant Med ADCM
#' rADCM <- radcm(cached = TRUE)
#' ADCM <- rADCM %>%
#'         select(USUBJID, STUDYID, ASTDTM, AENDTM, CMDECOD, ASTDY, AENDY)
#' ADCM <- left_join(ADSL, ADCM, by = c("USUBJID", "STUDYID"))
#'
#' # Laboratory ADLB
#' rADLB <- radlb(cached = TRUE)
#' ADLB <- rADLB %>%
#'         select(USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE, ADTM, ADY, ATPTN, AVISITN,
#'                LBTESTCD, ANRIND)
#' ADLB <- left_join(ADSL, ADLB, by = c("USUBJID", "STUDYID"))
#'
#' ADLB <- ADLB %>%
#'         group_by(USUBJID) %>%
#'         mutate(ANRIND = factor(ANRIND, levels = c("LOW", "NORMAL", "HIGH")))
#'
#'
#' # Example 1 "ADEX"
#' p1 <- patient_domain_profile(domain = "Exposure (ADEX)",
#'                              var_names = ADEX$PARCAT2,
#'                              marker_pos = ADEX$ASTDT_dur,
#'                              arrow_end = ADSL$max_day,
#'                              xtick_at = waiver(),
#'                              line_col = NULL,
#'                              line_col_legend = NULL,
#'                              line_col_opt = NULL,
#'                              line_width = 1,
#'                              arrow_size = 0.1,
#'                              no_enddate_extention = 0,
#'                              marker_color = factor(ADEX$Modification),
#'                              marker_color_opt =  c("Increase" = "red",
#'                              "Decrease" = "green", "None" = "blue"),
#'                              marker_color_legend = NULL,
#'                              marker_shape = factor(ADEX$Modification),
#'                              marker_shape_opt = c("Increase" = 24, "Decrease" = 25, "None" = 23),
#'                              marker_shape_legend = "Dose Modification",
#'                              show_days_label = TRUE,
#'                              xlim = c(-28, ADSL$max_day),
#'                              xlab = "Study Day",
#'                              title = paste("Patient Profile: ", ADSL$USUBJID))
#' p1
#'
#' # Example 2 "ADAE"
#' # Note that ASTDY is represented by a circle and AENDY is represented by a square.
#' # If AENDY and ASTDY occur on the same day only AENDY will be shown.
#'
#' p2 <- patient_domain_profile(domain = "Adverse Event (ADAE)",
#'                              var_names = ADAE$AEDECOD,
#'                              marker_pos = ADAE[, c("ASTDY", "AENDY")],
#'                              arrow_end = ADSL$max_day,
#'                              xtick_at = waiver(),
#'                              line_col = ADAE$AESER,
#'                              line_col_legend = "Serious",
#'                              line_col_opt = NULL,
#'                              line_width = 1,
#'                              arrow_size = 0.1,
#'                              no_enddate_extention = 0,
#'                              marker_color = factor(ADAE$AETOXGR),
#'                              marker_color_opt =  c("3" = "yellow", "4" = "red"),
#'                              marker_color_legend = NULL,
#'                              marker_shape = NULL,
#'                              marker_shape_opt = NULL,
#'                              marker_shape_legend = "Grade",
#'                              show_days_label = TRUE,
#'                              xlim = c(-28, ADSL$max_day),
#'                              xlab = "Study Day",
#'                              title = paste("Patient Profile: ", ADSL$USUBJID))
#'
#' p2
#'
#' # Example 3 "ADRS"
#' p3 <- patient_domain_profile(domain = "Tumor Response (ADRS)",
#'                              var_names = ADRS$PARAMCD,
#'                              marker_pos = ADRS$ADY,
#'                              arrow_end = ADSL$max_day,
#'                              xtick_at = waiver(),
#'                              line_col = NULL,
#'                              line_col_legend = NULL,
#'                              line_col_opt = NULL,
#'                              line_width = 1,
#'                              arrow_size = 0.1,
#'                              no_enddate_extention = 0,
#'                              marker_color = factor(ADRS$AVALC),
#'                              marker_color_opt =  c("CR" = "green", "PR" = "blue",
#'                              "SD" = "yellow", "PD" = "red", "NE" = "pink",
#'                              "Y" = "lightblue", "N" = "darkred"),
#'                              marker_color_legend = NULL,
#'                              marker_shape = factor(ADRS$AVALC),
#'                              marker_shape_opt = c("CR" = 21, "PR" = 24,
#'                              "SD" = 23, "PD" = 22, "NE" = 14,
#'                              "Y" = 11, "N" = 8),
#'                             marker_shape_legend = "Response",
#'                              show_days_label = TRUE,
#'                              xlim = c(-28, ADSL$max_day),
#'                              xlab = "Study Day",
#'                              title = paste("Patient Profile: ", ADSL$USUBJID))
#'p3
#'
#' # Example 4 "ADCM"
#' p4 <- patient_domain_profile(domain = "Concomitant Med (ADCM)",
#'                              var_names = ADCM$CMDECOD,
#'                              marker_pos = ADCM[, c("ASTDY", "AENDY")],
#'                              arrow_end = ADSL$max_day,
#'                              xtick_at = waiver(),
#'                              line_col = NULL,
#'                              line_col_legend = NULL,
#'                              line_col_opt = "orange",
#'                              line_width = 1,
#'                              arrow_size = 0.1,
#'                              no_enddate_extention = 50,
#'                              marker_color = NULL,
#'                              marker_color_opt = "orange",
#'                              marker_color_legend = NULL,
#'                              marker_shape = NULL,
#'                              marker_shape_opt = NULL,
#'                              marker_shape_legend = NULL,
#'                              show_days_label = TRUE,
#'                              xlim = c(-28, ADSL$max_day),
#'                              xlab = "Study Day",
#'                              title = paste("Patient Profile: ", ADSL$USUBJID))
#' p4
#'
#' # Example 5 "ADLB"
#' p5 <- patient_domain_profile(domain = "Laboratoy (ADLB)",
#'                              var_names = ADLB$LBTESTCD,
#'                              marker_pos = ADLB$ADY,
#'                              arrow_end = ADSL$max_day,
#'                              xtick_at = waiver(),
#'                              line_col = NULL,
#'                              line_col_legend = NULL,
#'                              line_col_opt = NULL,
#'                              line_width = 1,
#'                              arrow_size = 0.1,
#'                              no_enddate_extention = 0,
#'                              marker_color = factor(ADLB$ANRIND),
#'                              marker_color_opt =  c("HIGH" = "red", "LOW" = "blue",
#'                                                    "NORMAL" = "green", "NA" = "green"),
#'                              marker_color_legend = NULL,
#'                              marker_shape = factor(ADLB$ANRIND),
#'                              marker_shape_opt = c("HIGH" = 24, "LOW" = 25,
#'                                                   "NORMAL" = 23, "NA" = 23),
#'                              marker_shape_legend = "Labs Abnormality",
#'                              show_days_label = TRUE,
#'                              xlim = c(-30, ADSL$max_day),
#'                              xlab = "Study Day",
#'                              title = paste("Patient Profile: ", ADSL$USUBJID))
#'p5

patient_domain_profile <- function(domain = NULL,
                                   var_names,
                                   marker_pos,
                                   arrow_end,
                                   xtick_at = waiver(),
                                   line_col = NULL,
                                   line_col_legend = NULL,
                                   line_col_opt = NULL,
                                   line_width = 1,
                                   arrow_size = 0.1,
                                   no_enddate_extention = 0,
                                   marker_color = NULL,
                                   marker_color_opt = NULL,
                                   marker_color_legend = NULL,
                                   marker_shape = NULL,
                                   marker_shape_opt = NULL,
                                   marker_shape_legend = NULL,
                                   show_days_label = TRUE,
                                   xlim = c(-28, max(marker_pos) + 5),
                                   xlab = NULL,
                                   show_title = TRUE,
                                   title = NULL) {

  #check user input
  stop_if_not(
    list(length(unique(nrow(data.frame(var_names)), nrow(data.frame(marker_pos)), nrow(data.frame(arrow_end)))) == 1,
         "invalid arguments: check that the length of input arguments are identical"),
    list(ncol(data.frame(marker_pos)) <= 2,
         "invalid argument: check that marker_pos is either a vector or a data frame with two columns"),
    list(is.null(line_col) || length(line_col) == length(var_names),
         "invalid arguments: check that the length of line_col is equal as other inputs"),
    list(is.null(marker_color) || length(marker_color) == length(var_names),
         "invalid arguments: check that the length of marker_color is equal as other inputs"),
    list(is.null(marker_shape) || length(marker_shape) == length(var_names),
         "invalid arguments: check that the length of marker_shape is equal as other inputs"),
    list(is_numeric_vector(xlim, min_length = 2, max_length = 2),
         "invalid arguments: check that xlim is of type numeric vector")
  )

  marker_data <- data.frame(
    var_names,
    marker_pos = if (is.null(marker_pos))
      to_n("x", length(var_names)) else marker_pos,
    marker_shape = if (is.null(marker_shape))
      to_n("x", length(var_names)) else marker_shape,
    marker_color = if (is.null(marker_color))
      to_n("x", length(var_names)) else marker_color
    )

  # plot lines
  if (length(dim(marker_pos)) == 2) {
    line_data <- data.frame(
      var_names,
      line_col = if (is.null(line_col)) to_n("x", length(var_names)) else line_col,
      line_start = marker_pos[, 1],
      line_end = marker_pos[, 2],
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
          color = line_col),
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
        arrow = arrow(length = unit(arrow_size, "inches")),
        na.rm = TRUE
        )

    if (!is.null(line_col_opt)) {
      p <- p +
        scale_color_manual(
          breaks = line_data$line_col,
          values = line_col_opt,
          limits = levels(line_data$line_col)
          )
    } else {
      p <- p +
        scale_color_manual(
          breaks = line_data$line_col,
          values = c(1:25),
          limits = levels(line_data$line_col))
    }

    if (!is.null(line_col)) {
      p <- p + guides(color = guide_legend(line_col_legend, order = 1))
    } else {
      p <- p + guides(color = FALSE)
    }

    # plot markers
    p <- p +
      geom_point(
        data = marker_data,
        aes(x = var_names, y = marker_data[, 2], fill = factor(marker_color)),
        shape = 21,
        size = 5,
        na.rm = TRUE
        ) +
      geom_point(
        data = marker_data,
        aes(x = var_names, y = marker_data[, 3], fill = factor(marker_color)),
        shape = 22,
        size = 3,
        na.rm = TRUE
        )

    if (!is.null(marker_color_opt)) {
      p <- p +
        scale_fill_manual(
          breaks = marker_data$marker_color,
          values = marker_color_opt
          )
    } else {
      p <- p +
        scale_fill_manual(
          breaks = marker_data$marker_color,
          values = c(1:25))
    }


    p <- p + theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      ylab(xlab) + xlab(domain)

    if (!is.null(marker_color)) {
      p <- p + guides(fill = guide_legend(marker_color_legend, order = 2))
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
          fill = marker_color
          ),
                 size = 3, na.rm = TRUE) +
      scale_y_continuous(limits = xlim, breaks = xtick_at, expand = c(0, 0)) +
      coord_flip(xlim = c(1, length(unique(var_names)))) +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      ylab(xlab) + xlab(domain)

    if (is.null(marker_color_legend)) {
      if (length(setdiff(marker_color, marker_shape)) == 0) {
        marker_color_legend <- marker_shape_legend
      }
    }

    if (is.null(marker_shape_legend)) {
      if (length(setdiff(marker_color, marker_shape)) == 0) {
        marker_shape_legend <- marker_color_legend
      }
    }

    if (!is.null(marker_color_opt)) {
      p <- p +
        scale_fill_manual(
          name = marker_color_legend,
          breaks = marker_data$marker_color,
          values = marker_color_opt)
    } else {
      p <- p +
        scale_fill_manual(
          name = marker_color_legend,
          breaks = marker_data$marker_color,
          values = c(1:25)
          )
    }

    if (!is.null(marker_shape_opt)) {
      p <- p + scale_shape_manual(
        name = marker_shape_legend,
        breaks = marker_data$marker_shape,
        values = marker_shape_opt
        )
    } else {
      p <- p +
        scale_shape_manual(
          name = marker_shape_legend,
          breaks = marker_data$marker_shape,
          values = c(1:25)
          )
    }

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
        legend.spacing.y = unit(0, "cm"),
        legend.key.height = unit(1, "line"),
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
        legend.spacing.y = unit(0, "cm"),
        legend.key.height = unit(1, "line"),
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
#' If there is a missing dataset, assign a \code{NULL} value to it. ie) \code{ADRS <- NULL} prior to running the
#' patient profile plot.
#'
#' @param select_ex boolean value for showing \code{ADEX} domain plot, default is \code{TRUE}
#' @param select_ae boolean value for showing \code{ADAE} domain plot, default is \code{TRUE}
#' @param select_rs boolean value for showing \code{ADRS} domain plot, default is \code{TRUE}
#' @param select_cm boolean value for showing \code{ADCM} domain plot, default is \code{TRUE}
#' @param select_lb boolean value for showing \code{ADLB} domain plot, default is \code{TRUE}
#' @param ex_data dataframe for \code{ADEX} domain dataset
#' @param ex_var vector to identify each lane of \code{ADEX} domain plot
#' @param ae_data dataframe for \code{ADAE} domain dataset
#' @param ae_var vector to identify each lane of \code{ADAE} plot
#' @param ae_line_col factor vector to specify color for segments of \code{ADAE} plot
#' @param ae_line_col_legend string to be displayed as line color legend title of \code{ADAE} plot,
#' default is \code{NULL}
#' @param ae_line_col_opt aesthetic values to map line color values of \code{ADAE} plot
#'      (named vector to map color values to each name).
#'      If not \code{NULL}, please make sure this contains all posible values for \code{ae_line_col} values,
#'      otherwise color will be assigned by \code{ggplot} default, please note that \code{NULL} needs to be
#'      specified
#' @param rs_data dataframe for \code{ADRS} domain dataset
#' @param rs_var vector to identify each lane of \code{ADRS} domain plot
#' @param cm_data dataframe for \code{ADCM} domain dataset
#' @param cm_var vector to identify each lane of \code{ADCM} domain plot
#' @param lb_data dataframe for \code{ADLB} domain dataset
#' @param lb_var vector to identify each lane of \code{ADLB} domain plot
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
#' @importFrom cowplot plot_grid
#'
#' @return plot object
#'
#' @export
#'
#' @seealso \code{\link{patient_domain_profile}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(utils.nest)
#' library(tern)
#' library(dplyr)
#'
#' # ADSL
#' rADSL <- radsl(cached = TRUE)
#' ADSL <-  rADSL %>%
#'   group_by(USUBJID) %>%
#'   mutate(TRTSDT = as.Date(TRTSDTM),
#'          max_date = max(as.Date(LSTALVDT), as.Date(DTHDT)),
#'          max_day = as.numeric(as.Date(max_date) - as.Date(TRTSDT)) + 1) %>%
#'   select(USUBJID, STUDYID, TRTSDT , max_day) %>%
#'   filter(USUBJID == rADSL$USUBJID[1])
#'
#' # ADEX
#' rADEX <- radex(cached = TRUE)
#' ADEX <- rADEX %>%
#'         select(USUBJID, STUDYID, ASTDTM, PARCAT2, AVAL, AVALU, PARAMCD)
#' ADEX <- left_join(ADSL, ADEX, by = c("USUBJID", "STUDYID"))
#'
#' ADEX <- ADEX %>%
#'   filter(PARAMCD == "DOSE") %>%
#'   arrange(PARCAT2, PARAMCD) %>%
#'   mutate(diff = c(0, diff(AVAL, lag = 1))) %>%
#'   mutate(Modification = case_when(diff < 0 ~ "Decrease",
#'                                   diff > 0 ~ "Increase",
#'                                   diff == 0 ~ "None")) %>%
#'   mutate(ASTDT_dur = as.numeric(as.Date(substr(as.character(ASTDTM), 1, 10)) -
#'                                 as.Date(TRTSDT) + 1))
#'
#' # ADAE
#' rADAE <- radae(cached = TRUE)
#' ADAE <- rADAE %>%
#'         select(USUBJID, STUDYID, AESOC, AEDECOD, AESER, AETOXGR, AEREL, ASTDY, AENDY)
#' ADAE <- left_join(ADSL, ADAE, by = c("USUBJID", "STUDYID"))
#'
#' # ADRS
#' rADRS <- radrs(cached = TRUE)
#' ADRS <- rADRS %>%
#'         select(USUBJID, STUDYID, PARAMCD, PARAM, AVALC, AVAL, ADY, ADTM)
#' ADRS <- left_join(ADSL, ADRS, by = c("USUBJID", "STUDYID"))
#'
#' # ADCM
#' rADCM <- radcm(cached = TRUE)
#' ADCM <- rADCM %>%
#'         select(USUBJID, STUDYID, ASTDTM, AENDTM, CMDECOD, ASTDY, AENDY)
#' ADCM <- left_join(ADSL, ADCM, by = c("USUBJID", "STUDYID"))
#'
#' # ADLB
#' rADLB <- radlb(cached = TRUE)
#' ADLB <- rADLB %>%
#'         select(USUBJID, STUDYID, LBSEQ, PARAMCD, BASETYPE, ADTM, ADY, ATPTN, AVISITN,
#'                LBTESTCD, ANRIND)
#' ADLB <- left_join(ADSL, ADLB, by = c("USUBJID", "STUDYID"))
#'
#' ADLB <- ADLB %>%
#'         group_by(USUBJID) %>%
#'         mutate(ANRIND = factor(ANRIND, levels = c("LOW", "NORMAL", "HIGH")))
#'
#' # Example Patient Profile plot 5 domains
#' g_patient_profile(select_ex = TRUE,
#'                   select_ae = TRUE,
#'                   select_rs = TRUE,
#'                   select_cm = TRUE,
#'                   select_lb = TRUE,
#'                   ex_data = ADEX,
#'                   ex_var = ADEX$PARCAT2,
#'                   ae_data = ADAE,
#'                   ae_var = ADAE$AEDECOD,
#'                   ae_line_col = factor(ADAE$AESER),
#'                   ae_line_col_legend = "Serious",
#'                   ae_line_col_opt = c("Y" = "red", "N" = "blue"),
#'                   rs_data = ADRS,
#'                   rs_var = ADRS$PARAMCD,
#'                   cm_data = ADCM,
#'                   cm_var = ADCM$CMDECOD,
#'                   lb_data = ADLB,
#'                   lb_var = ADLB$LBTESTCD,
#'                   arrow_end_day = ADSL$max_day,
#'                   xlim = c(-28, ADSL$max_day),
#'                   xlab = "Study Day",
#'                   title = paste("Patient Profile: ", ADSL$USUBJID))
#'
#' # Example Patient Profile plot without ADCM and ADLB
#' g_patient_profile(select_ex = TRUE,
#'                   select_ae = TRUE,
#'                   select_rs = TRUE,
#'                   select_cm = FALSE,
#'                   select_lb = FALSE,
#'                   ex_data = ADEX,
#'                   ex_var = ADEX$PARCAT2,
#'                   ae_data = ADAE,
#'                   ae_var = ADAE$AEDECOD,
#'                   ae_line_col = factor(ADAE$AESER),
#'                   ae_line_col_legend = "Serious",
#'                   ae_line_col_opt = c("Y" = "red", "N" = "blue"),
#'                   rs_data = ADRS,
#'                   rs_var = ADRS$PARAMCD,
#'                   cm_data = NULL,
#'                   cm_var = NULL,
#'                   lb_data = NULL,
#'                   lb_var = NULL,
#'                   arrow_end_day = ADSL$max_day,
#'                   xlim = c(-28, ADSL$max_day),
#'                   xlab = "Study Day",
#'                   title = paste("Patient Profile: ", ADSL$USUBJID))
#'
g_patient_profile <- function(select_ex = TRUE,
                              select_ae = TRUE,
                              select_rs = TRUE,
                              select_cm = TRUE,
                              select_lb = TRUE,
                              ex_data,
                              ex_var,
                              ae_data,
                              ae_var,
                              ae_line_col,
                              ae_line_col_legend = NULL,
                              ae_line_col_opt = NULL,
                              rs_data,
                              rs_var,
                              cm_data,
                              cm_var,
                              lb_data,
                              lb_var,
                              arrow_end_day,
                              xlim = c(-28, 365),
                              xlab = "Study Day",
                              title = "Patient Profile") {
  stop_if_not(
    list(
      lapply(list(select_ex, select_ae, select_rs, select_cm, select_lb), rlang::is_bool) %>%
        unlist %>%
        all,
      "invalid argument: check that the select arguments are boolean")
  )

  # check if we have data for each of these plots
  if (dim(ex_data)[1] == 0 || is.null(ex_data)) {
    select_ex <- FALSE
    warning("No ADEX data for this subject")

  }

  if (dim(ae_data)[1] == 0 || is.null(ae_data)) {
    select_ae <- FALSE
    warning("No ADAE data for this subject")

  }

  if (dim(rs_data)[1] == 0 || is.null(rs_data)) {
    select_rs <- FALSE
    warning("No ADRS data for this subject")

  }

  if (dim(cm_data)[1] == 0 || is.null(cm_data)) {
    select_cm <- FALSE
    warning("No ADCM data for this subject")
  }
  if (dim(lb_data)[1] == 0 || is.null(lb_data)) {
    select_lb <- FALSE
    warning("No ADLB data for this subject")
  }

  select_list <- c(select_ex, select_ae, select_rs, select_cm, select_lb)


  show_days_label <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  show_days_label[max(which(select_list == TRUE))] <- TRUE

  show_title <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  show_title[min(which(select_list == TRUE))] <- TRUE

  # Domain "ADEX"
  if (select_ex == TRUE) {
    p1 <- patient_domain_profile(
      domain = "Exposure (ADEX)",
      var_names = ex_var,
      marker_pos = ex_data$ASTDT_dur,
      arrow_end = arrow_end_day,
      xtick_at = waiver(),
      line_col = NULL,
      line_col_legend = NULL,
      line_col_opt = NULL,
      line_width = 1,
      arrow_size = 0.1,
      no_enddate_extention = 0,
      marker_color = factor(ex_data$Modification),
      marker_color_opt =  c("Increase" = "red", "Decrease" = "green", "None" = "blue"),
      marker_color_legend = NULL,
      marker_shape = factor(ex_data$Modification),
      marker_shape_opt = c("Increase" = 24, "Decrease" = 25, "None" = 23),
      marker_shape_legend = "Dose Modification",
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
  if (select_ae == TRUE) {
    p2 <- patient_domain_profile(
       domain = "Adverse Event (ADAE)",
       var_names = ae_var,
       marker_pos = ae_data[, c("ASTDY", "AENDY")],
       arrow_end = arrow_end_day,
       xtick_at = waiver(),
       line_col = ae_line_col,
       line_col_legend = ae_line_col_legend,
       line_col_opt = ae_line_col_opt,
       line_width = 1,
       arrow_size = 0.1,
       no_enddate_extention = 0.1,
       marker_color = factor(ae_data$AETOXGR),
       marker_color_opt = c("1" = "green", "2" = "blue",
                            "3" = "yellow", "4" = "orange", "5" = "red"),
       marker_color_legend = "Grade",
       marker_shape = NULL,
       marker_shape_opt = NULL,
       marker_shape_legend = NULL,
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
  if (select_rs == TRUE) {
    p3 <- patient_domain_profile(
       domain = "Response (ADRS)",
       var_names = rs_var,
       marker_pos = rs_data$ADY,
       arrow_end = arrow_end_day,
       xtick_at = waiver(),
       line_col = NULL,
       line_col_legend = NULL,
       line_col_opt = NULL,
       line_width = 1,
       arrow_size = 0.1,
       no_enddate_extention = 0,
       marker_color = factor(rs_data$AVALC),
       marker_color_opt =  c("CR" = "green", "PR" = "blue", "SD" = "yellow",
                             "PD" = "red", "NE" = "pink", "Y" = "lightblue", "N" = "darkred"),
       marker_color_legend = NULL,
       marker_shape = factor(rs_data$AVALC),
       marker_shape_opt = c("CR" = 21, "PR" = 24, "SD" = 23, "PD" = 22, "NE" = 14,
                            "Y" = 11, "N" = 8),
       marker_shape_legend = "Response",
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
  if (select_cm == TRUE) {
    p4 <- patient_domain_profile(
       domain = "Conmed (ADCM)",
       var_names = cm_var,
       marker_pos = cm_data[, c("ASTDY", "AENDY")],
       arrow_end = arrow_end_day,
       xtick_at = waiver(),
       line_col = NULL,
       line_col_legend = NULL,
       line_col_opt = "orange",
       line_width = 1,
       arrow_size = 0.1,
       no_enddate_extention = 0.1,
       marker_color = NULL,
       marker_color_opt = "orange",
       marker_color_legend = NULL,
       marker_shape = NULL,
       marker_shape_opt = NULL,
       marker_shape_legend = NULL,
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
  if (select_lb == TRUE) {
    p5 <- patient_domain_profile(
       domain = "Laboratory (ADLB)",
       var_names = lb_var,
       marker_pos = lb_data$ADY,
       arrow_end = arrow_end_day,
       xtick_at = waiver(),
       line_col = NULL,
       line_col_legend = NULL,
       line_col_opt = NULL,
       line_width = 1,
       arrow_size = 0.1,
       no_enddate_extention = 0,
       marker_color = factor(lb_data$ANRIND),
       marker_color_opt =  c("HIGH" = "red", "LOW" = "blue",
                             "NORMAL" = "green"),
       marker_color_legend = NULL,
       marker_shape = factor(lb_data$ANRIND),
       marker_shape_opt = c("HIGH" = 24, "LOW" = 25, "NORMAL" = 23),
       marker_shape_legend = "Labs Abnormality",
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

  plot_list <- plot_list[select_list]
  # distribute space by number of levels in each domain
  var_list <- list(ex_var, ae_var, rs_var,
                   cm_var, lb_var)
  var_list <- var_list %>%
    lapply(unique) %>%
    lapply(length) %>%
    unlist %>%
    cbind(.data, select_list) %>%
    as.data.frame() %>%
    # keep the selected domains
    dplyr::filter(select_list == TRUE) %>%
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
