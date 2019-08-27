

#' Create a spiderplot_simple
#'
#' Descr of this plot
#'
#' @param anl The analysis data frame (e.g. ATE.sas7bdat on BCE)
#' @param byvar Analysis dataset
#' @param days Variable with time in days
#' @param mesValue Variable with measurement
#' @param groupCol Variable to color the individual lines and id in plot
#' @param baseday Numeric Value, pts with only smaller values will be cut out
#'
#'
#' @return ggplot object
#'
#' @importFrom rlang .data
#' @export
#'
#' @author Mika Maekinen
#'
#' @examples
#' library(random.cdisc.data)
#'
#' atr <- left_join(rADRS, rADSL)
#'
#' spiderplot_simple(atr %>% filter(PARAMCD == "SUMTGLES"), groupCol = "SEX")
spiderplot_simple <- function(anl, byvar = "USUBJID", days = "TRTDURD",
    mesValue = "PARAM", groupCol = "USUBJID", baseday = 0) {
  ### remove patients without post baseline measurement
  anl <- anl %>%
    group_by(!!byvar) %>%
    mutate(morebase = ifelse(max(!!days, na.rm = TRUE) > baseday, TRUE, FALSE)) %>%
    filter(.data$morebase == TRUE) %>%
    ungroup()
  ### find the last measurement
  lastObs <- anl %>% group_by(!!as.symbol(byvar)) %>% slice(which.max(!!as.symbol(days)))

  # plotr
  ggplot(data = anl, mapping = aes_string(x = days, y = mesValue, group = byvar, colour = groupCol),
          size = 2, alpha = 1) +
    geom_point(size = 3) + geom_line(size = 2, alpha = 0.7) +
    geom_text(aes_string(x = days, y = mesValue, label = byvar), data = lastObs, hjust = 0) +
    geom_hline(aes(yintercept = 0), linetype = "dotted", color = "black") +
    xlab("Time (Days)") +
    ylab("Change(%) from Baseline")# +
    # expand_limits(anl, x = select_(anl, mesValue) * 1.2)
}
