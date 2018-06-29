#' Create a butterfly plot for Early Development Visualization
#'
#'
#' @param category vector of y values
#' @param groups vector of dichotomization values
#' @param block_count string - what to count by (ex: # of AE or # of patients)
#' @param block_color color coding of bar segments
#' @param id unique subject identifier variable.
#' @param facet_rows vector defines what variable is used to split the
#' plot into rows, default here is NULL
#' @param x_label string of text for x axis label, default is block_count
#' @param y_label string of text for y axis label, default is AE Derived Terms
#' @param show_legend boolean of whether color coding legend is included,
#' default here is FALSE
#'
#' @return ggplot object
#'
#' @import lemon
#'
#' @export
#'
#' @author Carolyn Zhang
#'
#' @examples
#' library(random.cdisc.data)
#' library(plyr)
#' library(dplyr)
#' library(lemon)
#' library(reshape2)
#'
#' data <- left_join(radam("AAE", N=10),radam("ADSL", N=10))
#' #data <- data %>% filter(AEBODSYS %in% c("Vascular disorders", "Surgical and medical procedures"))
#'
#' p <- g_butterfly(category = data$AEBODSYS,
#'             groups = data$SEX,
#'             block_count = "# of patients",
#'             block_color = data$AETOXGR,
#'             id = data$USUBJID,
#'             facet_rows = data$RACE,
#'             x_label = block_count,
#'             y_label = "AE Derived Terms",
#'             legend_label = "AETOXGR",
#'             show_legend = TRUE)
#' p
#'

g_butterfly <- function(category,
                        groups,
                        block_count = "# of patients",
                        block_color = NULL,
                        id,
                        facet_rows = NULL,
                        x_label = block_count,
                        y_label = "AE Derived Terms",
                        legend_label = "AETOXGR",
                        show_legend = TRUE){

  #set up data-------
  if(is.null(facet_rows)){
    dat <- data.frame(id = id, y = category, groups = groups, bar_color = block_color)

    if(block_count == "# of patients"){
      counts <- dat %>% group_by(id, y, groups) %>%
        filter(bar_color == max(bar_color)) %>%
        distinct %>% group_by(y, groups) %>%
        arrange(y, groups) %>%
        tally %>%
        as.data.frame()
      in_dat <- data.frame(id = id, y = category, groups = groups, bar_color = block_color)
      temp <- in_dat %>%
        group_by(id, y, groups) %>%
        filter(bar_color == max(bar_color))%>% distinct %>% as.data.frame()
      temp <- temp[,-1]
    } else if(block_count == "# of AEs"){
      counts <- dat %>% group_by(y, groups) %>% tally %>% as.data.frame()
      temp <- data.frame(id = id, y = category, groups = groups, bar_color = block_color)
      temp <- temp[,-1]
    }

    temp$bar_color <- factor(temp$bar_color)

    counts <- left_join(counts, temp)
    max_c <- max(counts$n)
    counts$n0 <- rep(1, nrow(counts))
    counts <- counts %>% arrange(desc(bar_color))
    counts <- ddply(counts, c("y", "groups"), transform, label_ypos=cumsum(n0))
    counts <- ddply(counts, c("y", "groups", "bar_color"), transform, bar_count=sum(n0))

    text_ann <- counts %>% arrange(bar_color) %>%
                group_by(y, groups, bar_color) %>%
                filter(label_ypos == max(label_ypos))
  } else{
    dat <- data.frame(id = id, y = category, groups = groups, bar_color = block_color, f_rows = facet_rows)

    if(block_count == "# of patients"){
      counts <- dat %>% group_by(id, y, groups, f_rows) %>%
        filter(bar_color == max(bar_color)) %>%
        distinct %>% group_by(y, groups, f_rows) %>%
        arrange(y, groups, f_rows) %>%
        tally %>%
        as.data.frame()
      in_dat <- data.frame(id = id, y = category, groups = groups, bar_color = block_color, f_rows = facet_rows)
      temp <- in_dat %>%
        group_by(id, y, groups, f_rows) %>%
        filter(bar_color == max(bar_color))%>% distinct %>% as.data.frame()
      temp <- temp[,-1]
    } else if(block_count == "# of AEs"){
      counts <- dat %>% group_by(y, groups, f_rows) %>% tally %>% as.data.frame()
      temp <- data.frame(id = id, y = category, groups = groups, bar_color = block_color, f_rows = facet_rows)
      temp <- temp[,-1]
    }

    temp$bar_color <- factor(temp$bar_color)

    counts <- left_join(counts, temp)
    max_c <- max(counts$n)
    counts$n0 <- rep(1, nrow(counts))
    counts <- counts %>% arrange(desc(bar_color))
    counts <- ddply(counts, c("y", "groups", "f_rows"), transform, label_ypos=cumsum(n0))
    counts <- ddply(counts, c("y", "groups", "bar_color", "f_rows"), transform, bar_count=sum(n0))

    text_ann <- counts %>% arrange(bar_color) %>%
                group_by(y, groups, bar_color, f_rows) %>%
                filter(label_ypos == max(label_ypos))
  }

  g1 <- unique(as.character(groups))[1]
  g2 <- unique(as.character(groups))[2]

  #plot butterfly plot --------------------
  pl <- ggplot(counts, aes(x=y)) +
    geom_bar(data=counts[counts$groups==g1,], aes(y=n0, fill=bar_color), stat="identity") +
    geom_bar(data=counts[counts$groups==g2,], aes(y=-n0, fill=bar_color), stat="identity") +
    geom_hline(yintercept=0, colour="black", lwd=0.4) +
    geom_text(data=text_ann[text_ann$groups==g1,], aes(y = label_ypos, label = bar_count), hjust=1) +
    geom_text(data=text_ann[text_ann$groups==g2,], aes(y = -label_ypos, label = bar_count), hjust=-1) +
    annotate("text", x = counts$y[nrow(counts)], y = max(counts$label_ypos), label = g1) +
    annotate("text", x = counts$y[nrow(counts)], y = -max(counts$label_ypos), label = g2) +
    coord_flip() +
    scale_y_continuous(labels = abs, limits = (max_c) * c(-1,1)) +
    labs(x = y_label, y = block_count, fill = legend_label)

  if(!is.null(facet_rows)){
    pl <- pl + facet_grid(f_rows ~.)
  }

  if(show_legend){
    pl <- pl + theme_classic() +
      theme(strip.background = element_rect(colour = "white", fill = "white"),
            text = element_text(size = 20),
            axis.text = element_text(color = "black", size = 9),
            legend.text=element_text(size=9),
            legend.title = element_text(size = 12),
            panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
            strip.text = element_text(size = 5))
  } else{
    pl <- pl + theme_classic() +
      theme(strip.background = element_rect(colour = "white", fill = "white"),
            text = element_text(size = 20),
            axis.text = element_text(color = "black", size = 9),
            legend.text=element_text(size=9),
            legend.title = element_text(size = 12),
            panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
            strip.text = element_text(size = 5),
            legend.position = "none")
  }

  pl

}


