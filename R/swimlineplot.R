#' Create a swimlineplot
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
#' with(ASL, swimlineplot(WEIGHT, BAGE, ARM))
#'
#' swimlineplot(x = ASL$WEIGHT, y = ASL$BAGE, col = ASL$ARM)
#'
#'
swimlineplot <- function(x, y, col) {

  df <- data.frame(x = x, y = y, col = col)

  df %>%  ggplot() + aes(x, y, color = col) + geom_point()

}
