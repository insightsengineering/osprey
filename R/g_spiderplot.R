#' Create a spiderplot for Early Development Visualization
#'
#'
#' @param x_label string of text for x axis label
#' @param y_label string of text for y axis label
#' @param marker_x vector of x values
#' @param marker_y vector of y values
#' @param marker_color vector defines by what variable points are color coded,
#' , default here is NULL
#' @param marker_color_opt vector defines marker color code, default here is NULL
#' @param marker_shape vector defines by what variable points are shape coded,
#' , default here is NULL
#' @param marker_shape_opt vector defines marker shape code, default here is NULL
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
#' @param show_legend boolean of whether marker legend is included,
#' default here is FALSE
#'
#' @return ggplot object
#'
#' @export
#'
#' @author Carolyn Zhang
#'
#' @examples
#' library(random.cdisc.data)
#' library(plyr)
#' library(dplyr)
#'
#' atr <- left_join(radam("ATR", N=10),radam("ADSL", N=10))
#' dat <- atr %>% filter(PARAMCD == "SUMTGLES")
#'
#' cmp <- max(dat["TUDY"])
#' lbl <- apply(dat, 1, function(dat){if(dat["TUDY"] == cmp){dat["USUBJID"]}else{""}})
#' colors <- c("black", "red", "blue", "green", "yellow", "brown")
#' shapes <- c(0, 1, 2, 3, 4, 5, 6)
#' map_marker_color <- mapvalues(dat$RACE, from = levels(dat$RACE), to = colors[1:nlevels(dat$RACE)])
#' map_marker_shape <- mapvalues(dat$RACE, from = levels(dat$RACE), to = shapes[1:nlevels(dat$RACE)])
#' g_spiderplot(x_label = "Time (Days)",
#'              y_label = "Change (%) from Baseline",
#'              marker_x = dat$TUDY,
#'              marker_y = dat$PCHG,
#'              marker_color = dat$RACE,
#'              marker_color_opt = map_marker_color,
#'              marker_shape = dat$RACE,
#'              marker_shape_opt = map_marker_shape,
#'              line_color = dat$USUBJID,
#'              datalabel_txt = lbl,
#'              facet_rows = dat$SEX,
#'              facet_columns = dat$ARM,
#'              vref_line = c(10, 37),
#'              href_line = -0.3,
#'              show_legend = FALSE)
#'
g_spiderplot <- function(x_label, y_label, marker_x, marker_y, marker_color = NULL, marker_color_opt = NULL, marker_shape = NULL, marker_shape_opt = NULL, line_color, datalabel_txt = NULL, facet_rows = NULL, facet_columns = NULL, vref_line = NULL, href_line = NULL, show_legend = FALSE){

  #check length of input parameters
  #len_all <- c(length(marker_x), length(marker_y), length(marker_color), length(marker_shape), length(line_color), length(datalabel_txt))
  #if(length(unique(len_all)) > 1)
  #  stop("Input mismatch - check the length of your input parameters")

  #set up data-------
  dat <- data.frame(day = marker_x, pchg = marker_y, l_col = line_color)
  if(!is.null(marker_color)){
    dat$m_col <- marker_color
  }
  if(!is.null(marker_shape)){
    dat$sh <- marker_shape
  }
  if(!is.null(facet_rows)){
    dat$f_rows <- facet_rows
  }
  if(!is.null(facet_columns)){
    dat$f_columns <- facet_columns
  }

  #plot spider plot-----------------
  pl <- ggplot(data = dat, aes(x = dat$day, y = dat$pchg)) +
    geom_line(aes(color = dat$l_col), size = 2, alpha = 0.5, show.legend = FALSE)  +
    xlab(x_label) +
    ylab(y_label) +
    theme(legend.position="top", legend.title = element_blank())

  #marker shape and color------------
  # if(!is.null(marker_color) && !is.null(marker_shape) && !is.null(marker_color_opt)){
  #   pl <- pl + geom_point(aes(shape = dat$sh), colour = marker_color_opt, size = 3, show.legend = show_legend)
  # }
  # else if(!is.null(marker_color) && !is.null(marker_shape) && is.null(marker_color_opt)){
  #   pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), size = 3, show.legend = show_legend)
  # }
  # else if(!is.null(marker_color) && is.null(marker_shape) && !is.null(marker_color_opt)){
  #   pl <- pl + geom_point(colour = marker_color_opt, size = 3, show.legend = show_legend)
  # }
  # else if(!is.null(marker_color) && is.null(marker_shape) && is.null(marker_color_opt)){
  #   pl <- pl + geom_point(aes(color = dat$m_col), size = 3, show.legend = show_legend)
  # }
  # else if(is.null(marker_color) && !is.null(marker_shape)){
  #   pl <- pl + geom_point(aes(shape = dat$sh), size = 3, show.legend = show_legend)
  # }

  if(!is.null(marker_color) && !is.null(marker_shape)){
    if(!is.null(marker_color_opt) && !is.null(marker_shape_opt)){
      pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), shape = marker_shape_opt, colour = marker_color_opt, size = 3, show.legend = show_legend)
    }
    else if(!is.null(marker_color_opt) && is.null(marker_shape_opt)){
      pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), colour = marker_color_opt, size = 3, show.legend = show_legend)
    }
    else if(is.null(marker_color_opt) && !is.null(marker_shape_opt)){
      pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), shape = marker_shape_opt, size = 3, show.legend = show_legend)
    }
    else{
      pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), size = 3, show.legend = show_legend)
    }
  }
  else if(!is.null(marker_color) && is.null(marker_shape)){
    if(!is.null(marker_color_opt)){
      pl <- pl + geom_point(color = marker_color_opt, size = 3, show.legend = show_legend)
    }
    else{
      pl <- pl + geom_point(aes(color = dat$m_color), size = 3, show.legend = show_legend)
    }

  }
  else if(is.null(marker_color) && !is.null(marker_shape)){
    if(!is.null(marker_shape_opt)){
      pl <- pl + geom_point(shape = marker_shape_opt, size = 3, show.legend = show_legend)
    }
    else{
      pl <- pl + geom_point(aes(shape = dat$sh), size = 3, show.legend = show_legend)
    }

  }
  else if(is.null(marker_color) && is.null(marker_shape)){
    pl <- pl + geom_point(size = 3, show.legend = show_legend)
  }

  #label at last point---------
  if(!is.null(datalabel_txt)){
    pl <- pl + geom_text(aes(x = dat$day+2, y =  dat$pchg, label= datalabel_txt), size = 2)
  }

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
    pl <- pl + facet_grid(. ~ f_columns)
  }
  else if(is.null(facet_columns) && !is.null(facet_rows)){
    pl <- pl + facet_grid(f_rows ~.)
  }
  else{
    pl <- pl + facet_grid(f_rows ~ f_columns)
  }

  pl

}
