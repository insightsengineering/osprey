#' Standard Arguments
#'
#' The documentation to this function lists all the arguments in `osprey`
#' that are used repeatedly to express an analysis.
#'
#' @details Although this function just returns `NULL` it has two uses. For
#' the `osprey` users it provides a documentation of arguments that are
#' commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the `roxygen` argument description with:
#' `@inheritParams argument_convention`
#'
#' @param arm (`factor`)\cr vector that contains arm information in analysis data.
#' For example, `ADAE$ACTARMCD`.
#' @param conf_level (`numeric`)\cr the confidence interval level, default is 0.95.
#' @param diff_ci_method (`character`)\cr the method used to calculate confidence interval.
#' Default is "wald". Possible choices are methods supported in \code{\link[DescTools]{BinomDiffCI}}.
#' @param fontsize (`numeric`)\cr font size for the plot. It is the size used in ggplot2 with
#' default unit "mm", if you want "points" you will need to divide the point number by
#' \code{ggplot2:::.pt}.
#' @param draw (`logical`)\cr whether to draw the plot.
#'
#' @name argument_convention
#' @keywords internal
NULL
