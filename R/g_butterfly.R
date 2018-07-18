
#' Butterfly Plot
#'
#'
#' The butterfly plot is often used in Early Development (ED) and is an opposed
#' barplot that shows instances of AEs or # of patients by category separated by
#' a dichotomization variable. Each bar can be color coded according
#' to a variable of choice and sorted according to either alphabetical order or the
#' maximum count.
#'
#'
#' @param category vector of y values
#' @param groups vector of dichotomization values
#' @param block_count string - what to count by (ex: # of AEs or # of patients)
#' @param block_color vector - color coding of bar segments
#' @param id unique subject identifier variable.
#' @param facet_rows vector defines what variable is used to split the
#' plot into rows, default here is \code{NULL}
#' @param x_label string of text for x axis label, default is block_count
#' @param y_label string of text for y axis label, default is "AE Derived Terms"
#' @param sort_by character string that defines the ordering of the class and term
#' variables in the output table,
#' options: "alphabetical" or "count", default here is set to "count"
#' @param show_legend boolean of whether color coding legend is included,
#' default here is \code{FALSE}
#'
#'
#' @details there is no equivalent STREAM output
#'
#' @return ggplot object
#'
#' @import stringr
#' @importFrom plyr ddply
#'
#' @export
#'
#' @template author_zhanc107
#'
#' @examples
#' library(dplyr)
#'
#' data("rADSL")
#' data("rADAE")
#' ADSL <- rADSL %>% select(USUBJID, STUDYID, SEX) %>% filter(SEX %in% c("F", "M"))
#' AAE <- rADAE %>% select(USUBJID, STUDYID, AEBODSYS, AETOXGR)
#'
#' ANL <- left_join(AAE, ADSL, by = c("USUBJID", "STUDYID"))
#' ANL <- na.omit(ANL)
#'
#' g_butterfly(category = ANL$AEBODSYS,
#'             groups = ANL$SEX,
#'             block_count = "# of patients",
#'             block_color = ANL$AETOXGR,
#'             id = ANL$USUBJID,
#'             x_label = "# of patients",
#'             y_label = "AE Derived Terms",
#'             legend_label = "AETOXGR",
#'             sort_by = "count",
#'             show_legend = TRUE)
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
                        sort_by = "alphabetical",
                        show_legend = TRUE){

  #check validity of input arguments-------------------------
  check_input_length <- c(nrow(data.frame(category)), nrow(data.frame(groups)), nrow(data.frame(id)))
  check_input_col <- c(ncol(data.frame(category)), ncol(data.frame(groups)), ncol(data.frame(id)))

  if(length(unique(check_input_length)) > 1)
    stop("invalid arguments: check that the length of input arguments are identical")
  if(length(unique(check_input_col)) > 1 || unique(check_input_col) != 1)
    stop("invalid arguments: check that the inputs have a single column")
  if(any(check_input_length == 0) || any(check_input_col == 0))
    stop("invalid arguments: check that inputs are not null")
  if(length(unique(as.character(groups))) != 2)
    stop("invalid arguments: groups can only have 2 unique values")

  check_input_class <- c(class(block_count), class(x_label), class(y_label), class(legend_label))
  if(any(check_input_class != "character"))
    stop("invalid arguments: check that appropriate parameters are strings")

  all_opt = c("# of patients", "# of AEs")
  if(!any(block_count %in% all_opt))
    stop("invalid arguments: please check that block_count input is one of two appropriate terms")

  #set up data-------
  if(!is.null(block_color)){
    if(length(unique(c(nrow(data.frame(block_color)),check_input_length))) > 1)
      stop("invalid arguments: check that the length of input arguments are identical")

    if(length(unique(c(ncol(data.frame(block_color)),check_input_col))) > 1 || unique(c(ncol(data.frame(block_color)),check_input_col)) != 1)
      stop("invalid arguments: check that the inputs have a single column")

    if(is.null(facet_rows)){
      dat <- data.frame(id = id, y = category, groups = groups, bar_color = block_color)

      if(block_count == "# of patients"){
        counts <- dat %>% group_by(id, y, groups) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color)) %>%
          distinct %>% group_by(y, groups) %>%
          arrange(y, groups) %>%
          tally %>%
          as.data.frame()
        in_dat <- data.frame(id = id, y = category, groups = groups, bar_color = block_color)
        temp <- in_dat %>%
          group_by(id, y, groups) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color))%>% distinct %>% as.data.frame()
        temp <- temp[,-1]
      } else if(block_count == "# of AEs"){
        counts <- dat %>% group_by(y, groups) %>% tally %>% as.data.frame()
        temp <- data.frame(id = id, y = category, groups = groups, bar_color = block_color)
        temp <- temp[,-1]
      }

      temp$bar_color <- factor(temp$bar_color)
      counts <- left_join(counts, temp, by = c("y", "groups"))
      max_c <- max(counts$n)
      counts$n0 <- rep(1, nrow(counts))
      counts <- counts %>% arrange(desc(bar_color))
      counts <- ddply(counts, c("y", "groups"), transform, label_ypos=cumsum(n0))
      counts <- ddply(counts, c("y", "groups", "bar_color"), transform, bar_count=sum(n0))
      text_ann <- counts %>% arrange(bar_color) %>%
        group_by(y, groups, bar_color) %>%
        filter(label_ypos == max(label_ypos))
      total_text_ann <- text_ann %>% group_by(y, groups) %>% filter(label_ypos == max(label_ypos))
    } else{
      if(length(unique(c(nrow(data.frame(facet_rows)),check_input_length))) > 1)
        stop("invalid arguments: check that the length of input arguments are identical")
      if(length(unique(c(ncol(data.frame(facet_rows)),check_input_col))) > 1 || unique(c(ncol(data.frame(facet_rows)),check_input_col)) != 1)
        stop("invalid arguments: check that the inputs have a single column")

      dat <- data.frame(id = id, y = category, groups = groups, bar_color = block_color, f_rows = facet_rows)

      if(block_count == "# of patients"){
        counts <- dat %>% group_by(id, y, groups, f_rows) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color)) %>%
          distinct %>% group_by(y, groups, f_rows) %>%
          arrange(y, groups, f_rows) %>%
          tally %>%
          as.data.frame()
        in_dat <- data.frame(id = id, y = category, groups = groups, bar_color = block_color, f_rows = facet_rows)
        temp <- in_dat %>%
          group_by(id, y, groups, f_rows) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color))%>% distinct %>% as.data.frame()
        temp <- temp[,-1]
      } else if(block_count == "# of AEs"){
        counts <- dat %>% group_by(y, groups, f_rows) %>% tally %>% as.data.frame()
        temp <- data.frame(id = id, y = category, groups = groups, bar_color = block_color, f_rows = facet_rows)
        temp <- temp[,-1]
      }

      temp$bar_color <- factor(temp$bar_color)
      counts <- left_join(counts, temp, by = c("y", "groups"))
      max_c <- max(counts$n)
      counts$n0 <- rep(1, nrow(counts))
      counts <- counts %>% arrange(desc(bar_color))
      counts <- ddply(counts, c("y", "groups", "f_rows"), transform, label_ypos=cumsum(n0))
      counts <- ddply(counts, c("y", "groups", "bar_color", "f_rows"), transform, bar_count=sum(n0))
      text_ann <- counts %>% arrange(bar_color) %>%
        group_by(y, groups, bar_color, f_rows) %>%
        filter(label_ypos == max(label_ypos))
      total_text_ann <- text_ann %>% group_by(y, groups, f_rows) %>% filter(label_ypos == max(label_ypos))
    }
  }else{
    if(is.null(facet_rows)){
      dat <- data.frame(id = id, y = category, groups = groups)

      if(block_count == "# of patients"){
        counts <- dat %>% group_by(id, y, groups) %>%
          distinct %>% group_by(y, groups) %>%
          arrange(y, groups) %>%
          tally %>%
          as.data.frame()
        in_dat <- data.frame(id = id, y = category, groups = groups)
        temp <- in_dat %>%
          group_by(id, y, groups) %>%
          distinct %>% as.data.frame()
        temp <- temp[,-1]
      } else if(block_count == "# of AEs"){
        counts <- dat %>% group_by(y, groups) %>% tally %>% as.data.frame()
        temp <- data.frame(id = id, y = category, groups = groups)
        temp <- temp[,-1]
      }

      counts <- left_join(counts, temp, by = c("y", "groups"))
      max_c <- max(counts$n)
      counts$n0 <- rep(1, nrow(counts))
      counts <- ddply(counts, c("y", "groups"), transform, label_ypos=cumsum(n0))
      text_ann <- counts %>%
        group_by(y, groups) %>%
        filter(label_ypos == max(label_ypos))
      total_text_ann <- text_ann %>% group_by(y, groups) %>% filter(label_ypos == max(label_ypos))
    } else{
      if(length(unique(c(nrow(data.frame(facet_rows)),check_input_length))) > 1)
        stop("invalid arguments: check that the length of input arguments are identical")
      if(length(unique(c(ncol(data.frame(facet_rows)),check_input_col))) > 1 || unique(c(ncol(data.frame(facet_rows)),check_input_col)) != 1)
        stop("invalid arguments: check that the inputs have a single column")

      dat <- data.frame(id = id, y = category, groups = groups, f_rows = facet_rows)

      if(block_count == "# of patients"){
        counts <- dat %>% group_by(id, y, groups, f_rows) %>%
          distinct %>% group_by(y, groups, f_rows) %>%
          arrange(y, groups, f_rows) %>%
          tally %>%
          as.data.frame()
        in_dat <- data.frame(id = id, y = category, groups = groups, f_rows = facet_rows)
        temp <- in_dat %>%
          group_by(id, y, groups, f_rows) %>%
          distinct %>% as.data.frame()
        temp <- temp[,-1]
      } else if(block_count == "# of AEs"){
        counts <- dat %>% group_by(y, groups, f_rows) %>% tally %>% as.data.frame()
        temp <- data.frame(id = id, y = category, groups = groups, f_rows = facet_rows)
        temp <- temp[,-1]
      }

      counts <- left_join(counts, temp, by = c("y", "groups"))
      max_c <- max(counts$n)
      counts$n0 <- rep(1, nrow(counts))
      counts <- ddply(counts, c("y", "groups", "f_rows"), transform, label_ypos=cumsum(n0))
      text_ann <- counts %>%
        group_by(y, groups, f_rows) %>%
        filter(label_ypos == max(label_ypos))
      total_text_ann <- text_ann %>% group_by(y, groups, f_rows) %>% filter(label_ypos == max(label_ypos))
    }
  }

  counts$y <- str_wrap(counts$y, width = 30)
  total_text_ann$y <- str_wrap(total_text_ann$y, width = 30)
  text_ann$y <- str_wrap(text_ann$y, width = 30)

  if(sort_by == "alphabetical"){
    counts$y <- factor(counts$y, levels = rev(unique(counts$y[order(counts$y, decreasing = TRUE)])))

  } else if(sort_by == "count"){
    tot <- counts %>% group_by(y, groups) %>%
                     filter(label_ypos == last(label_ypos)) %>%
                     group_by(y) %>%
                     tally(n) %>%
                     data.frame
    counts <- left_join(counts, tot, by = "y")
    counts$y <- factor(counts$y, levels = rev(unique(counts$y[order(counts[, ncol(counts)], decreasing = TRUE)])))
  }

  g1 <- unique(as.character(groups))[1]
  g2 <- unique(as.character(groups))[2]

  #plot butterfly plot --------------------
  if(!is.null(block_color)){
    pl <- ggplot(counts, aes(x=y)) +
      geom_bar(data=counts[counts$groups==g1,], aes(y=n0, fill=bar_color), stat="identity") +
      geom_bar(data=counts[counts$groups==g2,], aes(y=-n0, fill=bar_color), stat="identity") +
      geom_hline(yintercept=0, colour="black", lwd=0.4) +
      geom_text(data=text_ann[text_ann$groups==g1,], aes(y = label_ypos, label = bar_count), hjust=0.9) +
      geom_text(data=text_ann[text_ann$groups==g2,], aes(y = -label_ypos, label = bar_count), hjust=-0.9) +
      geom_text(data=total_text_ann[total_text_ann$groups==g1,], aes(y = label_ypos, label = n), hjust=-0.9) +
      geom_text(data=total_text_ann[total_text_ann$groups==g2,], aes(y = -label_ypos, label = n), hjust=0.9) +
      coord_flip() +
      scale_y_continuous(labels = abs, limits = (max_c*1.2) * c(-1,1)) +
      labs(x = y_label, y = block_count, fill = legend_label)
  } else {
    pl <- ggplot(counts, aes(x=y)) +
      geom_bar(data=counts[counts$groups==g1,], aes(y=n0), stat="identity") +
      geom_bar(data=counts[counts$groups==g2,], aes(y=-n0), stat="identity") +
      geom_hline(yintercept=0, colour="black", lwd=0.4) +
      geom_text(data=total_text_ann[total_text_ann$groups==g1,], aes(y = label_ypos, label = n), hjust=-0.9) +
      geom_text(data=total_text_ann[total_text_ann$groups==g2,], aes(y = -label_ypos, label = n), hjust=0.9) +
      coord_flip() +
      scale_y_continuous(labels = abs, limits = (max_c*1.2) * c(-1,1)) +
      labs(x = y_label, y = block_count, fill = legend_label)
  }

  if(!is.null(facet_rows)){
    pl <- pl + facet_wrap(~f_rows, ncol = 1)
  }

  if(show_legend){
    pl <- pl + theme_classic() +
      theme(strip.background = element_rect(colour = "white", fill = "white"),
            strip.text.x = element_text(color = "black", size = 9),
            title  = element_text(size = 9),
            axis.title = element_text(size = 20),
            axis.text = element_text(color = "black", size = 9),
            legend.text=element_text(size=9),
            legend.title = element_text(size = 9),
            panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
            plot.margin=unit(c(1.5,1,1,1),"cm"),
            strip.text = element_text(size = 5))
  } else{
    pl <- pl + theme_classic() +
      theme(strip.background = element_rect(colour = "white", fill = "white"),
            strip.text.x = element_text(color = "black", size = 9),
            title  = element_text(size = 9),
            axis.title = element_text(size = 20),
            axis.text = element_text(color = "black", size = 9),
            legend.text=element_text(size=9),
            legend.title = element_text(size = 9),
            panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
            strip.text = element_text(size = 5),
            plot.margin=unit(c(1.5,1,1,1),"cm"),
            legend.position = "none")
  }

  if(sort_by == "alphabetical"){
    pl <- pl + scale_x_discrete(limits = rev(levels(factor(counts$y))))
  } else if(sort_by == "count"){
    pl <- pl + scale_x_discrete(limits = levels(factor(counts$y)))
  }

  pl <- pl + labs(title = str_wrap(g2, width = 30))
  g <- ggplotGrob(pl)

  g2 <- gtable_add_grob(g, textGrob(str_wrap(g1, width = 30), x=1, just = "right", hjust=1, gp=gpar(fontsize = 11)),
                        t=2, l=4, b=2, r=4, name="right-title")
  grid.draw(g2)

}


