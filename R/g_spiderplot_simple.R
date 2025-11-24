#' Simple spider plot
#'
#' Description of this plot
#'
#' @param anl The analysis data frame
#' @param byvar Analysis dataset
#' @param days Variable with time in days
#' @param mes_value Variable with measurement
#' @param group_col Variable to color the individual lines and id in plot
#' @param baseday Numeric Value, points with only smaller values will be cut out
#'
#'
#' @return `ggplot` object
#'
#' @export
#'
#' @author Mika Maekinen
#'
#' @examplesIf require("nestcolor")
#' library(dplyr)
#' library(nestcolor)
#'
#' ADSL <- osprey::rADSL[1:15, ]
#' ADTR <- osprey::rADTR
#' ANL <- left_join(ADSL, ADTR)
#'
#' ANL %>%
#'   dplyr::filter(ANL01FL == "Y" & PARAMCD == "SLDINV") %>%
#'   spiderplot_simple(group_col = "SEX", days = "ADY", mes_value = "AVAL")
spiderplot_simple <- function(anl,
                              byvar = "USUBJID",
                              days = "TRTDURD",
                              mes_value = "PARAM",
                              group_col = "USUBJID",
                              baseday = 0) {
  ### remove patients without post baseline measurement
  anl <- anl %>%
    group_by(!!byvar) %>%
    dplyr::mutate(morebase = ifelse(max(!!days, na.rm = TRUE) > baseday, TRUE, FALSE)) %>%
    dplyr::filter(.data$morebase == TRUE) %>%
    ungroup()
  ### find the last measurement
  last_obs <- anl %>%
    group_by(!!as.symbol(byvar)) %>%
    slice(which.max(!!as.symbol(days)))

  # plotr
  ggplot(
    data = anl,
    mapping = aes(x = .data[[days]],
                  y = .data[[mes_value]],
                  group = .data[[byvar]],
                  colour = .data[[group_col]]),
  ) +
    geom_point(size = 3) +
    geom_line(linewidth = 2, alpha = 0.7) +
    geom_text(aes(x = .data[[days]],
                  y = .data[[mes_value]],
                  label = .data[[byvar]]), data = last_obs, hjust = 0) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black") +
    xlab("Time (Days)") +
    ylab("Change(%) from Baseline")
}
