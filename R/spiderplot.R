

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
#' ASL <- radam("ASL", start_with = list(WEIGHT = c(80, 85, 90), STUDYID = "A"))
#'
#' with(ASL, spiderplot(WEIGHT, BAGE, ARM))
#'
#' spiderplot(x = ASL$WEIGHT, y = ASL$BAGE, col = ASL$ARM)
#'
#'
spiderplot <- function(x, y, col) {

  df <- data.frame(x = x, y = y, col = col)

  df %>%  ggplot() + aes(x, y, color = col) + geom_point()

}
