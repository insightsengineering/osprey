

#' Create a spiderplot
#'
#' Descr of this plot
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param col color of points
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
#' ATE <- radam("ATE", N=10 ,start_with = list(CHANGE = c(0, 80, 85, 90), STUDYID = c("A","B")))
#'
#' with(ATE, spiderplot(AVAL, CHANGE, USUBJID, STUDYID))
#'
#' spiderplot(x = ATE$AVAL, y = ATE$CHANGE, group= ATE$USUBJID,color = ATE$STUDYID)
#'
#'
spiderplot <- function(x, y, group, color) {

  df <- data.frame(x = x, y = y, group = group, color=color)

  df %>%  ggplot() + aes(x, y, group = group, color = color) +
    geom_point() +
    geom_line()
}

