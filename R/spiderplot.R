

#' Create a spiderplot
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
#' spiderplot(atr)
#'
#'
spiderplot <- function(anl) {

  ##plotr
    ggplot(data=anl, mapping = aes(x = TUDY, y = PCHG, group = USUBJID, colour=STUDYID),size=2, alpha = 1) +
    geom_point(size=3) + geom_line(size=2, alpha=0.7) +
    geom_text(aes(x = TRDY, y = PCHG, label=plotLabel), data=lastObs, hjust = 0) +
    geom_hline(aes(yintercept = 0), linetype="dotted" , color = "black") +
    scale_colour_manual(name="Overall Best Response", values = respcolours()) +
    xlab("Time (Days)") +
    ylab("Change(%) from Baseline") +
    expand_limits(x = anl$TRDY * 1.2)
}

