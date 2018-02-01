
#' Create A Waterfall plot
#'
#' A waterfall plot shows...
#'
#' @inheritParams spiderplot
#'
#' @return a ggplot object
#'
#' @export
#'
#' @examples
#' waterfall(1:5, 5:1)
waterfall <- function(x, y) {

  data.frame(A = x, B = y) %>%
    ggplot() + aes(A, B) %>%
    geom_point()

}
