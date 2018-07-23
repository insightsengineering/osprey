
#' Spider Plot
#'
#' Spider plot is often used in Early Development (ED) and displays individual
#' patient plot of an endpoint over time by group.
#'
#'
#' @param marker_x dataframe with 2 columns,
#' column 1 is the vector of x values (must be in sorted order) and
#' column 2 is the vector to group the points together (default
#' should be USUBJID)
#' @param marker_y vector of y values
#' @param line_colby vector defines by what variable plot is color coded,
#' default here is \code{NULL}
#' @param marker_size size of markers in plot, default here is \code{NULL}
#' @param marker_shape vector defines by what variable points are shape coded,
#' , default here is \code{NULL}
#' @param marker_shape_opt vector defines marker shape code, default here is \code{NULL}
#' @param datalabel_txt list defines text (at last time point) and
#' flag for an arrow annotation
#' (per defined variable) - elements must be labeled txt_ann/mrkr_all/mrkr_ann
#' txt_ann - text annotation next to final data point (for text annotation)
#' mrkr_all - vector of ID's (for annotation marker)
#' mrkr_ann - vector of ID's (subset of mrkr_all) where arrow is desired to
#' indicate any study interim points
#' @param facet_rows vector defines what variable is used to split the
#' plot into rows, default here is \code{NULL}
#' @param facet_columns vector defines what variable is used to split the
#' plot into columns, default here is \code{NULL}
#' @param vref_line value defines vertical line overlay
#' (can be a vector), default here is \code{NULL}
#' @param href_line value defines horizontal line overlay
#' (can be a vector), default here is NULL
#' @param x_label string of text for x axis label, default is time
#' @param y_label string of text for y axis label, default is % change
#' @param show_legend boolean of whether marker legend is included,
#' default here is \code{FALSE}
#'
#' @return ggplot object
#'
#' @details there is no equivalent STREAM output
#'
#' @export
#'
#' @template author_zhanc107
#'
#' @examples
#' # simple example
#' library(dplyr)
#'
#' data("rADSL")
#' data("rADTR")
#' ADTR <- rADTR %>% select(STUDYID, USUBJID, ADY, PCHG, PARAMCD)
#' ADSL <- rADSL %>% select(STUDYID, USUBJID, RACE, SEX, ARM)
#' ANL <- left_join(ADTR, ADSL, by = c("STUDYID", "USUBJID"))
#' ANL <- ANL %>% filter(PARAMCD == "SLDINV") %>% filter(RACE %in% c("WHITE", "ASIAN")) %>% group_by(USUBJID) %>% arrange(ADY)
#' ANL <- na.omit(ANL)
#' ANL$USUBJID <- substr(ANL$USUBJID, 14, 18)
#'
#'
#' p <- g_spiderplot(marker_x = data.frame(day = ANL$ADY, groupby = ANL$USUBJID),
#'              marker_y = ANL$PCHG,
#'              line_colby = ANL$USUBJID,
#'              marker_shape = ANL$RACE,
#'              marker_shape_opt = c("ASIAN" = 1, "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = 2,
#'                                   "BLACK OR AFRICAN AMERICAN" = 3, "WHITE" = 4,
#'                                   "AMERICAN INDIAN OR ALASKA NATIVE" = 5, "UNKNOWN" = 6),
#'              marker_size = 5,
#'              datalabel_txt = list(txt_ann = ANL$USUBJID),
#'              #datalabel_txt = list(txt_ann = ANL$USUBJID, mrkr_all = ANL$USUBJID, mrkr_ann = c("id-1", "id-4", "id-7")),
#'              #datalabel_txt = list(mrkr_all = ANL$USUBJID, mrkr_ann = c("id-2", "id-4", "id-7")),
#'              facet_rows = ANL$SEX,
#'              facet_columns = ANL$ARM,
#'              vref_line = c(10, 37),
#'              href_line = -0.3,
#'              x_label = "Time (Days)",
#'              y_label = "Change (%) from Baseline",
#'              show_legend = FALSE)
#' p
#'
#' \dontrun{
#' #test discrete x-axis points
#' dat2 <- dat %>% arrange(TUDY) %>% mutate(day = as.character(TUDY)) %>% as.data.frame()
#'}
g_spiderplot <- function(marker_x,
                         marker_y,
                         line_colby = NULL,
                         marker_shape = NULL,
                         marker_shape_opt = NULL,
                         marker_size = 3,
                         datalabel_txt = NULL,#USUBJID default
                         facet_rows = NULL,
                         facet_columns = NULL,
                         vref_line = NULL,
                         href_line = NULL,
                         x_label = "Time (Days)",
                         y_label = "Change (%) from Baseline",
                         show_legend = FALSE,
                         draw = TRUE,
                         newpage = TRUE){

  check_input_length <- c(nrow(data.frame(marker_x)), nrow(data.frame(marker_y)))

  if(length(unique(check_input_length)) > 1)
    stop("invalid arguments: check that the length of input arguments are identical")
  if(ncol(marker_x) != 2 || ncol(data.frame(marker_y)) != 1)
    stop("invalid arguments: check that the inputs have the correct numner of columns")
  if(any(check_input_length == 0))
    stop("invalid arguments: check that inputs are not null")

  #set up data-------
  dat <- data.frame(day = marker_x[, 1], pchg = marker_y, group = marker_x[, 2])

  if(!is.null(marker_shape)){
    if(length(unique(c(nrow(marker_shape), check_input_length))) != 1)
      stop("invalid arguments: check that the length of input arguments are identical")
    dat$sh <- marker_shape
  }
  if(!is.null(facet_rows)){
    if(length(unique(c(nrow(facet_rows), check_input_length))) != 1)
      stop("invalid arguments: check that the length of input arguments are identical")
    dat$f_rows <- facet_rows
  }
  if(!is.null(facet_columns)){
    if(length(unique(c(nrow(facet_columns), check_input_length))) != 1)
      stop("invalid arguments: check that the length of input arguments are identical")
    dat$f_columns <- facet_columns
  }
  if(!is.null(line_colby)){
    if(length(unique(c(nrow(line_colby), check_input_length))) != 1)
      stop("invalid arguments: check that the length of input arguments are identical")
    dat$l_col <- line_colby
  }
  if(!is.null(datalabel_txt$txt_ann)){
    dat$lbl_all <- datalabel_txt$txt_ann

    dat <- dat %>%
      group_by(lbl_all) %>%
      mutate(lab = ifelse(day == last(day), as.character(lbl_all), " "))
  }
  if(!is.null(datalabel_txt$mrkr_all) && !is.null(datalabel_txt$mrkr_ann)){
    if(length(unique(c(nrow(datalabel_txt$mrkr_all), check_input_length))) != 1)
      stop("invalid arguments: check that the length of input arguments are identical")
    dat$id <- datalabel_txt$mrkr_all
  }

  dat <- dat %>% as.data.frame()

  #plot spider plot-----------------
  pl <- ggplot(data = dat, aes(x = day, y = pchg, group = group)) +
    xlab(x_label) +
    ylab(y_label) +
    theme(legend.position="top", legend.title = element_blank())


  #line color
  if(!is.null(line_colby)){
    pl <- pl + geom_line(aes(color = l_col), size = 1, alpha = 0.5, show.legend = show_legend)
  } else{
    pl <- pl + geom_line(size = 1, alpha = 0.5, show.legend = show_legend)
  }

  #marker color------------
  if(!is.null(marker_shape)){
    pl <- pl + geom_point(aes(shape = sh, color = l_col), size = marker_size, show.legend = show_legend)
  } else if(is.null(marker_shape)){
    pl <- pl + geom_point(aes(color = l_col), size = 3, show.legend = show_legend)
  }

  #label at last data point---------
  if(!is.null(datalabel_txt)){

    if(!is.null(datalabel_txt$txt_ann) && is.null(datalabel_txt$mrkr_all) && is.null(datalabel_txt$mrkr_ann)){
      pl <- pl + geom_text(data = dat, aes(x = day, y =  pchg, label= lab), hjust = -0.3, size = 4, show.legend = FALSE)
    } else if(is.null(datalabel_txt$txt_ann) && !is.null(datalabel_txt$mrkr_all) && !is.null(datalabel_txt$mrkr_ann)){

      dat_arrow <- dat %>%
        filter(id %in% datalabel_txt$mrkr_ann) %>%
        group_by(id) %>%
        filter(day == last(day))
      pl <- pl + geom_segment(data = dat_arrow, mapping = aes(x = day, y = pchg, xend = day, yend = pchg), arrow = arrow(length = unit(0.15, "inches"), ends = "first", type = "closed"), size = 0.4, color = "black", show.legend = FALSE)

    } else if(!is.null(datalabel_txt$txt_ann) && !is.null(datalabel_txt$mrkr_all) && !is.null(datalabel_txt$mrkr_ann)){
      pl <- pl + geom_text(data = dat, aes(x = day, y =  pchg, label= lab), hjust = -0.45, size = 4, show.legend = FALSE)

      dat_arrow <- dat %>%
        filter(id %in% datalabel_txt$mrkr_ann) %>%
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
    for(i in 1:length(vref_line)){
      pl <- pl + annotate("segment", x = vref_line[i], y = -Inf, xend = vref_line[i], yend = Inf, linetype = "dotted", color = "black")
    }
  }

  #facets---------------
  if(is.null(facet_rows) && is.null(facet_columns)){
    pl
  } else if(is.null(facet_rows) && !is.null(facet_columns)){
    pl <- pl + facet_grid(.~ f_columns) # facet_rep_grid(.~ f_columns) - use rep to add in axis lines (require lemon)
  } else if(is.null(facet_columns) && !is.null(facet_rows)){
    pl <- pl + facet_grid(f_rows ~.)
  } else{
    pl <- pl + facet_grid(f_rows ~ f_columns)
  }

  call_color <- function(len){
    datCol <- data.frame(color_opt = colors())
    datCol <- datCol %>% filter(!grepl("white",color_opt)) %>% droplevels

    return(datCol[1:len, 1])
  }

  if(!is.null(marker_shape_opt)){
    pl <- pl + scale_shape_manual(name = "Shape",
                                  breaks = dat$sh,
                                  values = marker_shape_opt)
  }

  #modify background color
  pl <- pl + annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    theme_bw() +
    theme(strip.background = element_rect(colour = "white", fill = "white"),
          text = element_text(size = 16),
          axis.text = element_text(color = "black"),
          legend.text=element_text(size=7),
          legend.title = element_text(size = 7)) +
    labs(colour = "Color", shape = "Shape") +
    guides(color = guide_legend(override.aes = list(size=2)))

  if(is.numeric(marker_x[, 1])){
    pl <- pl + xlim(min(marker_x[, 1]), max(marker_x[, 1])*1.3)
  }else{
    pl <- pl + scale_x_discrete(expand = c(0.3, 0))
  }
  pl

}
