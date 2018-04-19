

#' Create a spiderplot_simple
#'
#' Descr of this plot
#'
#' @param anl Analysis dataset
#'
#' @return ggplot object
#'
#' @export
#'
#' @author Mika
#'
#' @examples
#' library(random.cdisc.data)
#'
#'
#' atr <- radam("ATR", N=10 ,start_with = list(CHANGE = c(0, 80, 85, 90), STUDYID = c("A","B")))
#'
#'
#' spiderplot_simple(atr %>% filter(PARAMCD == "SUMTGLES"))
#'
#'
spiderplot_simple <- function(anl) {

  ###remove patients without post baseline measurement
  anl <- anl %>% group_by(USUBJID) %>% mutate(morebase = ifelse(max(TUDY, na.rm=TRUE) > 0, TRUE, FALSE)) %>% filter(morebase == TRUE) %>% ungroup()
  ###find the last measurement
  lastObs <- anl  %>%  group_by(USUBJID) %>% slice(which.max(TUDY))

  ##plotr
    ggplot(data=anl, mapping = aes(x = TUDY, y = PCHG, group = USUBJID, colour=USUBJID),size=2, alpha = 1) +
    geom_point(size=3) + geom_line(size=2, alpha=0.7) +
    geom_text(aes(x = TUDY, y = PCHG, label=USUBJID), data=lastObs, hjust = 0) +
    geom_hline(aes(yintercept = 0), linetype="dotted" , color = "black") +
    xlab("Time (Days)") +
    ylab("Change(%) from Baseline") +
    expand_limits(x = anl$TUDY * 1.2)
}

