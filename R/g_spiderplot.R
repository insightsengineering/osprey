#' Create a spiderplot for Early Development Visualization
#'
#'
#' @param x_label string of text for x axis label
#' @param y_label string of text for y axis label
#' @param marker_x vector of x values
#' @param marker_y vector of y values
#' @param marker_color vector defines by what variable points are color coded
#' @param marker_shape vector defines by what variable points are shape coded
#' @param line_color vector defines by what variable plot is color coded
#' @param datalabel_txt vector defines text at last time point for each line
#' @param facet_rows vector defines what variable is used to split the
#' plot into rows, default here is NULL
#' @param facet_columns vector defines what variable is used to split the
#' plot into columns, default here is NULL
#' @param vref_line value defines vertical line overlay
#' (can be a vector), default here is NULL
#' @param href_line value defines horizontal line overlay
#' (can be a vector), default here is NULL
#'
#' @return ggplot object
#'
#' @export
#'
#' @author Carolyn Zhang
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' atr <- left_join(radam("ATR", N=10),radam("ADSL", N=10))
#' dat <- atr %>% filter(PARAMCD == "SUMTGLES")
#'
#' cmp <- max(dat["TUDY"])
#' temp <- apply(dat, 1, function(dat){if(dat["TUDY"] == cmp){dat["USUBJID"]}else{""}})
#' g_spiderplot(x_label = "Time (Days)",
#'              y_label = "Change (%) from Baseline",
#'              marker_x = dat$TUDY,
#'              marker_y = dat$PCHG,
#'              marker_color = dat$RACE,
#'              marker_shape = dat$RACE,
#'              line_color = dat$USUBJID,
#'              datalabel_txt = temp,
#'              facet_rows = dat$SEX,
#'              facet_columns = dat$ARM,
#'              vref_line = c(10, 37),
#'              href_line = -0.3)
#'
#'
#'
g_spiderplot <- function(x_label, y_label, marker_x, marker_y, marker_color, marker_shape, line_color, datalabel_txt, facet_rows = NULL, facet_columns = NULL, vref_line = NULL, href_line = NULL){

  #check length of input parameters
  len_all <- c(length(marker_x), length(marker_y), length(marker_color), length(marker_shape), length(line_color), length(datalabel_txt))
  if(length(unique(len_all)) > 1)
    stop("Input mismatch - check the length of your input parameters")

  #set up data-------
  if(is.null(facet_rows) && is.null(facet_columns)){
    dat <- data.frame(day = marker_x,
                      pchg = marker_y,
                      m_col = marker_color,
                      sh = marker_shape,
                      l_col = line_color)

  }
  else if(is.null(facet_rows) && !is.null(facet_columns)){
    dat <- data.frame(day = marker_x,
                      pchg = marker_y,
                      m_col = marker_color,
                      sh = marker_shape,
                      l_col = line_color,
                      f_columns = facet_columns)
  }
  else if(is.null(facet_columns) && !is.null(facet_rows)){
    dat <- data.frame(day = marker_x,
                      pchg = marker_y,
                      m_col = marker_color,
                      sh = marker_shape,
                      l_col = line_color,
                      f_rows = facet_rows)
  }
  else{
    dat <- data.frame(day = marker_x,
                      pchg = marker_y,
                      m_col = marker_color,
                      sh = marker_shape,
                      l_col = line_color,
                      f_rows = facet_rows,
                      f_columns = facet_columns)
  }


  #plot spider plot-----------------
  pl <- ggplot(data = dat, aes(x = dat$day, y = dat$pchg)) +
    geom_point(aes(color = dat$m_col, shape = dat$sh), size = 3, show.legend = TRUE) +
    geom_line(aes(color = dat$l_col), size = 2, alpha = 0.5, show.legend = FALSE)  +
    xlab(x_label) +
    ylab(y_label) +
    geom_text(aes(x = dat$day+2, y =  dat$pchg, label= datalabel_txt), size = 2) +
    theme(legend.position="top", legend.title = element_blank())

  if(!is.null(href_line)){
    pl <- pl + geom_hline(yintercept = href_line, linetype = "dotted", color = "black")
  }
  if(!is.null(vref_line)){
    pl <- pl + geom_vline(xintercept = vref_line, linetype = "dotted", color = "black")
  }

  #facets---------------
  if(is.null(facet_rows) && is.null(facet_columns)){
    pl
  }
  else if(is.null(facet_rows) && !is.null(facet_columns)){
    pl + facet_grid(. ~ f_columns)
  }
  else if(is.null(facet_columns) && !is.null(facet_rows)){
    pl + facet_grid(f_rows ~.)
  }
  else{
    pl + facet_grid(f_rows ~ f_columns)
  }


}
