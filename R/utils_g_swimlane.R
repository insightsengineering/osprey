to_n <- function(x, n) {
  if (is.null(x)) {
    NULL
  } else if (length(x)==1) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop("dimension missmatch")
  }
}
