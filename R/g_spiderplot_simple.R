#' Simple spider plot
#'
#' Description of this plot
#'
#' @param anl The analysis data frame (e.g. `ATE.sas7bdat` on `BCE`)
#' @param byvar Analysis dataset
#' @param days Variable with time in days
#' @param mes_value Variable with measurement
#' @param group_col Variable to color the individual lines and id in plot
#' @param baseday Numeric Value, `pts` with only smaller values will be cut out
#'
#'
#' @return `ggplot` object
#'
#' @export
#'
#' @author Mika Maekinen
#'
#' @examples
#' library(dplyr)
#' library(nestcolor)
#'
#' ADSL <- rADSL
#' ADRS <- rADRS
#' ANL <- left_join(ADSL, ADRS)
#'
#' ANL %>%
#'   dplyr::filter(PARAMCD == "OVRINV") %>%
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
    mapping = aes_string(x = days, y = mes_value, group = byvar, colour = group_col),
    size = 2,
    alpha = 1
  ) +
    geom_point(size = 3) +
    geom_line(size = 2, alpha = 0.7) +
    geom_text(aes_string(x = days, y = mes_value, label = byvar), data = last_obs, hjust = 0) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black") +
    xlab("Time (Days)") +
    ylab("Change(%) from Baseline")
}
