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
#' @param marker_size size of markers in plot
#' @param marker_shape vector defines by what variable points are shape coded,
#' , default here is NULL
#' @param marker_shape_opt vector defines marker shape code, default here is NULL
#' @param line_color_colby vector defines by what variable plot is color coded
#' @param datalabel_txt list defines text (at last time point) and flag for discontinued study
#' (per defined variable) - elements must be labeled one/two/three
#' one - text annotation next to final data point (for text annotation)
#' two - vector of ID's (for discontinued study marker)
#' three - vector of ID's (subset of two) where arrow is desired to indicate discontinued study
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
#' lbl <- apply(dat, 1, function(dat){if(dat["TUDY"] == cmp){paste(dat["USUBJID"], "test-length")}else{""}})
#' colors <- c("black", "red", "blue", "green", "yellow", "brown")
#' shapes <- c(0, 1, 2, 3, 4, 5, 6)
#' map_marker_color <- mapvalues(dat$RACE, from = levels(dat$RACE), to = colors[1:nlevels(dat$RACE)])
#' map_marker_shape <- mapvalues(dat$RACE, from = levels(dat$RACE), to = shapes[1:nlevels(dat$RACE)])
#' g_spiderplot(x_label = "Time (Days)",
#'              y_label = "Change (%) from Baseline",
#'              marker_x = dat$TUDY,
#'              marker_y = dat$PCHG,
#'              marker_color = dat$USUBJID,
#'              #marker_color_opt = map_marker_color,
#'              marker_shape = dat$RACE,
#'              #marker_shape_opt = map_marker_shape,
#'              marker_size = 5,
#'              line_color_colby = dat$USUBJID,
#'              #datalabel_txt = list(one = dat$USUBJID, two = dat$USUBJID, three = c("id-2", "id-4", "id-7")),
#'              datalabel_txt = list(two = dat$USUBJID, three = c("id-2", "id-4", "id-7")),
#'              facet_rows = dat$SEX,
#'              facet_columns = dat$ARM,
#'              vref_line = c(10, 37),
#'              href_line = -0.3,
#'              show_legend = FALSE)
#'
g_spiderplot <- function(x_label = "Time (Days)",
                         y_label = "Change (%) from Baseline",
                         marker_x,
                         marker_y,
                         marker_color = NULL,
                         marker_color_opt = NULL,
                         marker_shape = NULL,
                         marker_shape_opt = NULL,
                         marker_size = 6,
                         line_color_colby = NULL,
                         datalabel_txt = NULL,
                         facet_rows = NULL,
                         facet_columns = NULL,
                         vref_line = NULL,
                         href_line = NULL,
                         show_legend = FALSE,
                         draw = TRUE,
                         newpage = TRUE){

  #set up data-------
  dat <- data.frame(day = marker_x, pchg = marker_y)

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
  if(!is.null(line_color_colby)){
    dat$l_col <- line_color_colby
  }
  if(!is.null(datalabel_txt$one)){
    dat$lbl_all <- datalabel_txt$one
    dat <- dat %>%
      group_by(lbl_all) %>%
      mutate(dat, lab = ifelse(day == max(day), as.character(lbl_all), " "))
  }
  if(!is.null(datalabel_txt$two) && !is.null(datalabel_txt$three)){
    dat$id <- datalabel_txt$two
  }

  #plot spider plot-----------------
  pl <- ggplot(data = dat, aes(x = day, y = pchg)) +
    xlab(x_label) +
    ylab(y_label) +
    theme(legend.position="top", legend.title = element_blank())

  if(!is.null(line_color_colby)){
    pl <- pl + geom_line(aes(color = dat$l_col), size = 2, alpha = 0.5, show.legend = FALSE)
  }
  else{
    pl <- pl + geom_line(size = 2, alpha = 0.5, show.legend = FALSE)
  }

  #marker shape and color------------
  if(!is.null(marker_color) && !is.null(marker_shape)){
    if(!is.null(marker_color_opt) && !is.null(marker_shape_opt)){
      pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), shape = marker_shape_opt, colour = marker_color_opt, size = marker_size, show.legend = show_legend)
    }
    else if(!is.null(marker_color_opt) && is.null(marker_shape_opt)){
      pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), colour = marker_color_opt, size = marker_size, show.legend = show_legend)
    }
    else if(is.null(marker_color_opt) && !is.null(marker_shape_opt)){
      pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), shape = marker_shape_opt, size = marker_size, show.legend = show_legend)
    }
    else{
      pl <- pl + geom_point(aes(color = dat$m_col, shape = dat$sh), size = marker_size, show.legend = show_legend)
    }
  }
  else if(!is.null(marker_color) && is.null(marker_shape)){
    if(!is.null(marker_color_opt)){
      pl <- pl + geom_point(color = marker_color_opt, size = marker_size, show.legend = show_legend)
    }
    else{
      pl <- pl + geom_point(aes(color = dat$m_col), size = marker_size, show.legend = show_legend)
    }
  }
  else if(is.null(marker_color) && !is.null(marker_shape)){
    if(!is.null(marker_shape_opt)){
      pl <- pl + geom_point(shape = marker_shape_opt, size = marker_size, show.legend = show_legend)
    }
    else{
      pl <- pl + geom_point(aes(shape = dat$sh), size = marker_size, show.legend = show_legend)
    }
  }
  else if(is.null(marker_color) && is.null(marker_shape)){
    pl <- pl + geom_point(size = 3, show.legend = show_legend)
  }

  #label at last data point---------
  if(!is.null(datalabel_txt)){

    #datalabel_txt_mod <- gsub(" ", "\n ", lbl, fixed = TRUE)
    if(!is.null(datalabel_txt$one)){
      pl <- pl + geom_text(data = dat, aes(x = day+5, y =  pchg, label= lab), size = 5)
      pl <- pl + geom_text(data = dat, aes(x = dat$day+max(nchar(lab))+4, y =  dat$pchg+(max(dat$pchg)/10), label= ""), size = 6)
    }
    #pl <- pl + geom_text(data = dat_txt, mapping = aes(x = dat$day+3, y = dat$pchg, label = dat$id), size = 6)

    if(!is.null(datalabel_txt$two) && !is.null(datalabel_txt$three)){
      dat_arrow <- dat %>%
        filter(id %in% datalabel_txt$three) %>%
        group_by(id) %>%
        filter(day == max(day))

      pl <- pl + geom_segment(data = dat_arrow, mapping = aes(x = day-7, y = pchg, xend = day-2, yend = pchg), arrow = arrow(length = unit(0.25, "inches")), size = 1, color = "black")
    }
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

  #change fontsize
  pl <- pl + theme_linedraw() +
    theme(strip.text = element_text(colour = "black"), strip.background = element_rect(colour = "white", fill = "white"), text = element_text(size = 25))

  pl

}
