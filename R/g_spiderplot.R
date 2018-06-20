#' Create a spiderplot for Early Development Visualization
#'
#'
#' @param marker_x dataframe with 2 columns,
#' column 1 is the vector of x values and
#' column 2 is the vector to group the points together (default
#' should be defined as USUBJID)
#' @param marker_y vector of y values
#' @param line_colby vector defines by what variable plot is color coded,
#' default here is NULL
#' @param marker_color vector defines by what variable points are color coded,
#' , default here is NULL
#' @param marker_color_opt vector defines marker color code, default here is NULL
#' @param marker_size size of markers in plot, default here is NULL
#' @param marker_shape vector defines by what variable points are shape coded,
#' , default here is NULL
#' @param marker_shape_opt vector defines marker shape code, default here is NULL
#' @param datalabel_txt list defines text (at last time point) and
#' flag for an arrow annotation
#' (per defined variable) - elements must be labeled one/two/three
#' one - text annotation next to final data point (for text annotation)
#' two - vector of ID's (for annotation marker)
#' three - vector of ID's (subset of two) where arrow is desired to
#' indicate any study interim points
#' @param facet_rows vector defines what variable is used to split the
#' plot into rows, default here is NULL
#' @param facet_columns vector defines what variable is used to split the
#' plot into columns, default here is NULL
#' @param vref_line value defines vertical line overlay
#' (can be a vector), default here is NULL
#' @param href_line value defines horizontal line overlay
#' (can be a vector), default here is NULL
#' @param x_label string of text for x axis label, default is time
#' @param y_label string of text for y axis label, default is % change
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
#' require(lemon)
#'
#' atr <- left_join(radam("ATR", N=10),radam("ADSL", N=10))
#' dat <- atr %>% filter(PARAMCD == "SUMTGLES")
#'
#' #test changing where annotation marker lies
#' #dat <- dat[-4, ]
#'
#' colors <- c("black", "red", "blue", "green", "yellow", "brown")
#' shapes <- c(0, 1, 2, 3, 4, 5, 6)
#' #map_marker_color <- mapvalues(dat$RACE, from = levels(dat$RACE), to = colors[1:nlevels(dat$RACE)])
#' #map_marker_shape <- mapvalues(dat$RACE, from = levels(dat$RACE), to = shapes[1:nlevels(dat$RACE)])
#' g_spiderplot(marker_x = data.frame(day = dat$TUDY, groupby = dat$USUBJID),
#'              marker_y = dat$PCHG,
#'              line_colby = dat$RACE,
#'              marker_color = dat$RACE,
#'              marker_color_opt = c("ASIAN" = "yellow", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = "red",
#'                                   "BLACK OR AFRICAN AMERICAN" = "black", "WHITE" = "green",
#'                                   "AMERICAN INDIAN OR ALASKA NATIVE" = "blue"),
#'              marker_shape = dat$RACE,
#'              marker_shape_opt = c("ASIAN" = 1, "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = 2,
#'                                   "BLACK OR AFRICAN AMERICAN" = 3, "WHITE" = 4,
#'                                   "AMERICAN INDIAN OR ALASKA NATIVE" = 5),
#'              marker_size = 5,
#'              datalabel_txt = list(one = dat$USUBJID),
#'              #datalabel_txt = list(one = dat$USUBJID, two = dat$USUBJID, three = c("id-1", "id-4", "id-7")),
#'              #datalabel_txt = list(two = dat$USUBJID, three = c("id-2", "id-4", "id-7")),
#'              facet_rows = dat$SEX,
#'              facet_columns = dat$ARM,
#'              vref_line = c(10, 37),
#'              href_line = -0.3,
#'              x_label = "Time (Days)",
#'              y_label = "Change (%) from Baseline",
#'              show_legend = TRUE)
#'
#' #test discrete x-axis points
#' dat2 <- dat %>% arrange(TUDY) %>% mutate(day = as.character(TUDY)) %>% as.data.frame()
#' g_spiderplot(marker_x = data.frame(day = as.factor(dat2$day), groupby = dat2$USUBJID),
#'              marker_y = dat2$PCHG,
#'              line_colby = dat2$RACE,
#'              marker_color = dat2$USUBJID,
#'              #marker_color_opt = map_marker_color,
#'              marker_shape = dat2$RACE,
#'              #marker_shape_opt = map_marker_shape,
#'              marker_size = 5,
#'              datalabel_txt = list(one = dat2$USUBJID, two = dat2$USUBJID, three = c("id-2", "id-4", "id-7")),
#'              #datalabel_txt = list(two = dat2$USUBJID, three = c("id-2", "id-4", "id-7")),
#'              facet_rows = dat2$SEX,
#'              facet_columns = dat2$ARM,
#'              vref_line = c("10", "37"),
#'              href_line = -0.3,
#'              x_label = "Time (Days)",
#'              y_label = "Change (%) from Baseline",
#'              show_legend = FALSE)
#'
g_spiderplot <- function(marker_x,
                         marker_y,
                         line_colby = NULL,
                         marker_color = NULL,
                         marker_color_opt = NULL,
                         marker_shape = NULL,
                         marker_shape_opt = NULL,
                         marker_size = 6,
                         datalabel_txt = NULL,
                         facet_rows = NULL,
                         facet_columns = NULL,
                         vref_line = NULL,
                         href_line = NULL,
                         x_label = "Time (Days)",
                         y_label = "Change (%) from Baseline",
                         show_legend = FALSE,
                         draw = TRUE,
                         newpage = TRUE){

  #set up data-------

  dat <- data.frame(day = marker_x[, 1], pchg = marker_y)

  if(ncol(marker_x) == 2){
    dat <- data.frame(day = marker_x[, 1], pchg = marker_y, group = marker_x[, 2])
  }

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
  if(!is.null(line_colby)){
    dat$l_col <- line_colby
  }
  if(!is.null(datalabel_txt$one)){
    dat$lbl_all <- datalabel_txt$one

    if(ncol(marker_x) == 1){
      dat <- dat %>%
        group_by(lbl_all) %>%
        mutate(dat, lab = ifelse(day == max(day), as.character(lbl_all), " "))
    }
    else{
      dat <- dat %>%
        group_by(lbl_all) %>%
        mutate(dat, lab = ifelse(day == last(day), as.character(lbl_all), " "))
    }
  }
  if(!is.null(datalabel_txt$two) && !is.null(datalabel_txt$three)){
    dat$id <- datalabel_txt$two
  }

  dat <- dat %>% as.data.frame()

  #plot spider plot-----------------
  pl <- ggplot(data = dat, aes(x = day, y = pchg, group = group)) +
    xlab(x_label) +
    ylab(y_label) +
    theme(legend.position="top", legend.title = element_blank())


  #line color
  if(!is.null(line_colby)){
    pl <- pl + geom_line(aes(color = l_col), size = 2, alpha = 0.5, show.legend = FALSE)
  } else{
    pl <- pl + geom_line(size = 2, alpha = 0.5, show.legend = FALSE)
  }

  #marker shape and color------------
  if(!is.null(marker_color) && !is.null(marker_shape)){
      pl <- pl + geom_point(aes(color = m_col, shape = sh), size = marker_size, show.legend = show_legend)
  } else if(!is.null(marker_color) && is.null(marker_shape)){
      pl <- pl + geom_point(aes(color = m_col), size = marker_size, show.legend = show_legend)
  } else if(is.null(marker_color) && !is.null(marker_shape)){
      pl <- pl + geom_point(aes(shape = sh), size = marker_size, show.legend = show_legend)
  } else if(is.null(marker_color) && is.null(marker_shape)){
    pl <- pl + geom_point(size = 3, show.legend = show_legend)
  }

  #label at last data point---------
  if(!is.null(datalabel_txt)){

    if(!is.null(datalabel_txt$one) && is.null(datalabel_txt$two) && is.null(datalabel_txt$three)){
      pl <- pl + geom_text(data = dat, aes(x = day, y =  pchg, label= lab), hjust = -0.3, size = 4, show.legend = FALSE)
    } else if(is.null(datalabel_txt$one) && !is.null(datalabel_txt$two) && !is.null(datalabel_txt$three)){

      dat_arrow <- dat %>%
        filter(id %in% datalabel_txt$three) %>%
        group_by(id) %>%
        filter(day == last(day))
      pl <- pl + geom_segment(data = dat_arrow, mapping = aes(x = day, y = pchg, xend = day, yend = pchg), arrow = arrow(length = unit(0.15, "inches"), ends = "first", type = "closed"), size = 0.4, color = "black", show.legend = FALSE)

    } else if(!is.null(datalabel_txt$one) && !is.null(datalabel_txt$two) && !is.null(datalabel_txt$three)){
      pl <- pl + geom_text(data = dat, aes(x = day, y =  pchg, label= lab), hjust = -0.45, size = 4, show.legend = FALSE)

      dat_arrow <- dat %>%
        filter(id %in% datalabel_txt$three) %>%
        group_by(id) %>%
        filter(day == last(day))
      pl <- pl + geom_segment(data = dat_arrow, mapping = aes(x = day, y = pchg, xend = day, yend = pchg), arrow = arrow(length = unit(0.15, "inches"), ends = "first", type = "closed"), size = 0.4, color = "black", show.legend = FALSE)

    }
  }

  #vertical and horizontal reference lines
  if(!is.null(href_line)){
    pl <- pl + geom_hline(yintercept = href_line, linetype = "dotted", color = "black")
  }

  if(!is.null(vref_line)){
    if(ncol(marker_x) == 1){
      pl <- pl + geom_vline(xintercept = vref_line, linetype = "dotted", color = "black")
    } else{
      for(i in 1:length(vref_line)){
        pl <- pl + annotate("segment", x = vref_line[i], y = -Inf, xend = vref_line[i], yend = Inf, linetype = "dotted", color = "black")
      }
    }
  }

  #facets---------------
  if(is.null(facet_rows) && is.null(facet_columns)){
    pl
  } else if(is.null(facet_rows) && !is.null(facet_columns)){
    pl <- pl + facet_rep_grid(.~ f_columns)
  } else if(is.null(facet_columns) && !is.null(facet_rows)){
    pl <- pl + facet_rep_grid(f_rows ~.)
  } else{
    pl <- pl + facet_rep_grid(f_rows ~ f_columns)
  }

  print(dat$m_col)
  print(unique(dat$m_col))

  #marker and color options
  if(!is.null(marker_color_opt)){
    pl <- pl + scale_color_manual(name = "Marker Color",
                            breaks = dat$m_col,
                            values = marker_color_opt)
  }
  if(!is.null(marker_shape_opt)){
    pl <- pl + scale_shape_manual(name = "Marker Shape",
                                  breaks = dat$sh,
                                  values = marker_shape_opt)
  }

  #modify background color
  pl <- pl + theme_classic()+ theme(strip.background = element_rect(colour = "white", fill = "white"),
                                    text = element_text(size = 25),
                                    axis.text = element_text(color = "black"))

  if(is.numeric(marker_x[, 1])){
    pl <- pl + xlim(min(marker_x[, 1]), max(marker_x[, 1])*1.3)
  }else{
    pl <- pl + scale_x_discrete(expand = c(0.3, 0))
  }
  pl

}
