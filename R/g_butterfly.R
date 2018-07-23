
#' Butterfly Plot (version 2.0)
#'
#'
#' A modification of the deafult butterfly plot in which 2 flags can
#' be used as the dichotomization variables.
#'
#'
#' @param category vector of y values
#' @param rightFlag vector of 1/0 represents right side of barplot
#' @param leftFlag vector of 1/0 represents left side of barplot
#' @param group_names string vector of length 2 with desired names of dichotomization variables
#' required format : first name corresponds to the name of the right side
#'                   second name corresponds to name of the left side
#' default: will extract column names from group
#' @param block_count string - what to count by (ex: # of AEs or # of patients)
#' @param block_color vector - color coding of bar segments
#' @param id unique subject identifier variable.
#' @param facet_rows vector defines what variable is used to split the
#' plot into rows, default here is NULL
#' @param x_label string of text for x axis label, default is block_count
#' @param y_label string of text for y axis label, default is AE Derived Terms
#' @param sort_by character string that defines the ordering of the class and term
#' variables in the output table,
#' options: "alphabetical" or "count", default here is set to "count"
#' @param show_legend boolean of whether color coding legend is included,
#' default here is FALSE
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
#' ADSL <- rADSL %>% select(USUBJID, STUDYID, SEX, ARM, RACE) %>% filter(SEX %in% c("F", "M"))
#' AAE <- rADAE %>% select(USUBJID, STUDYID, AEBODSYS, AETOXGR)
#'
#' ANL <- left_join(AAE, ADSL, by = c("STUDYID", "USUBJID"))
#' ANL <- ANL %>% mutate(flag1 = ifelse(SEX == "F", 1, 0)) %>% mutate(flag2 = ifelse(SEX == "M", 1, 0))
#' ANL <- na.omit(ANL)
#'
#' g_butterfly(category = ANL$AEBODSYS,
#'             rightFlag = ANL$flag1,
#'             leftFlag = ANL$flag2,
#'             group_names = c("flag1", "flag2"),
#'             block_count = "# of patients",
#'             block_color = ANL$AETOXGR,
#'             id = ANL$USUBJID,
#'             #facet_rows = ANL$RACE,
#'             x_label = "# of patients",
#'             y_label = "AE Derived Terms",
#'             legend_label = "AETOXGR",
#'             sort_by = "count",
#'             show_legend = TRUE)
#'

g_butterfly <- function(category,
                        rightFlag,
                        leftFlag,
                        group_names = NULL,
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
  check_input_length <- c(nrow(data.frame(category)), nrow(data.frame(leftFlag)), nrow(data.frame(rightFlag)), nrow(data.frame(id)))
  check_input_col <- c(ncol(data.frame(category)), ncol(data.frame(id)))

  if(length(unique(check_input_length)) > 1)
    stop("invalid arguments: check that the length of input arguments are identical")
  if(length(unique(check_input_col)) > 1 || unique(check_input_col) != 1)
    stop("invalid arguments: check that the inputs have a single column")
  if(any(check_input_length == 0) || any(check_input_col == 0))
    stop("invalid arguments: check that inputs are not null")
  if(ncol(data.frame(leftFlag)) != 1 || ncol(data.frame(rightFlag)) != 1)
    stop("invalid arguments: groups must have two columns each representing one dichotomization variable")

  check_input_class <- c(class(block_count), class(x_label), class(y_label), class(legend_label))
  if(any(check_input_class != "character"))
    stop("invalid arguments: check that appropriate parameters are strings")

  all_opt = c("# of patients", "# of AEs")
  if(!any(block_count %in% all_opt))
    stop("invalid arguments: please check that block_count input is one of two appropriate terms (# of patients or # of AEs)")

  #set up data-------
  groups <- data.frame(flag1 = rightFlag, flag2 = leftFlag)

  if(length(unique(as.character(groups$flag1))) > 2 || length(unique(as.character(groups$flag2))) > 2)
    stop("invalid arguments: groups can only have 2 unique values")

  if(!is.null(block_color)){
    if(length(unique(c(nrow(data.frame(block_color)),check_input_length))) > 1)
      stop("invalid arguments: check that the length of input arguments are identical")

    if(length(unique(c(ncol(data.frame(block_color)),check_input_col))) > 1 || unique(c(ncol(data.frame(block_color)),check_input_col)) != 1)
      stop("invalid arguments: check that the inputs have a single column")

    if(is.null(facet_rows)){
      dat <- data.frame(id = id, y = category, flag1 = groups$flag1, flag2 = groups$flag2, bar_color = block_color)

      if(block_count == "# of patients"){
        counts1 <- dat %>% group_by(id, y, flag1, flag2) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color)) %>%
          distinct %>% group_by(y, flag1, flag2) %>%
          select(-id) %>%
          group_by(y, flag1) %>%
          tally %>%
          filter(flag1 == 1) %>%
          as.data.frame()
        counts2 <-  dat %>% group_by(id, y, flag1, flag2) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color)) %>%
          distinct %>% group_by(y, flag1, flag2) %>%
          select(-id) %>%
          group_by(y, flag2) %>%
          tally %>%
          filter(flag2 == 1) %>%
          as.data.frame()
        in_dat <- data.frame(id = id, y = category, flag1 = groups$flag1, flag2 = groups$flag2, bar_color = block_color)
        temp1 <- in_dat %>% select(-flag2) %>%
          group_by(id, y, flag1) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color))%>% distinct %>% as.data.frame()
        temp2 <- in_dat %>% select(-flag1) %>%
          group_by(id, y, flag2) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color))%>% distinct %>% as.data.frame()
        temp1 <- temp1[,-1]
        temp2 <- temp2[,-1]
      } else if(block_count == "# of AEs"){
        counts1 <- dat %>% group_by(y, flag1) %>% tally %>% as.data.frame()
        counts2 <- dat %>% group_by(y, flag2) %>% tally %>% as.data.frame()
        temp1 <- data.frame(id = id, y = category, flag1 = groups$flag1, bar_color = block_color)
        temp2 <- data.frame(id = id, y = category, flag2 = groups$flag2, bar_color = block_color)
        temp1 <- temp1[,-1]
        temp2 <- temp2[,-1]
      }

      temp1$bar_color <- factor(temp1$bar_color)
      temp2$bar_color <- factor(temp2$bar_color)
      counts1 <- left_join(counts1, temp1, by = c("y", "flag1"))
      counts2 <- left_join(counts2, temp2, by = c("y", "flag2"))
      max_c <- max(c(counts1$n, counts2$n))

      counts1$n0 <- rep(1, nrow(counts1))
      counts1 <- counts1 %>% arrange(desc(bar_color))
      counts1 <- ddply(counts1, c("y", "flag1"), transform, label_ypos=cumsum(n0))
      counts1 <- ddply(counts1, c("y", "flag1",  "bar_color"), transform, bar_count=sum(n0))
      text_ann1 <- counts1 %>% arrange(bar_color) %>%
        group_by(y, flag1,bar_color) %>%
        filter(label_ypos == max(label_ypos))
      total_text_ann1 <- text_ann1 %>% group_by(y, flag1) %>% filter(label_ypos == max(label_ypos))

      counts2$n0 <- rep(1, nrow(counts2))
      counts2 <- counts2 %>% arrange(desc(bar_color))
      counts2 <- ddply(counts2, c("y", "flag2"), transform, label_ypos=cumsum(n0))
      counts2 <- ddply(counts2, c("y", "flag2",  "bar_color"), transform, bar_count=sum(n0))
      text_ann2 <- counts2 %>% arrange(bar_color) %>%
        group_by(y, flag2,bar_color) %>%
        filter(label_ypos == max(label_ypos))
      total_text_ann2 <- text_ann2 %>% group_by(y, flag2) %>% filter(label_ypos == max(label_ypos))

    } else{
      if(length(unique(c(nrow(data.frame(facet_rows)),check_input_length))) > 1)
        stop("invalid arguments: check that the length of input arguments are identical")
      if(length(unique(c(ncol(data.frame(facet_rows)),check_input_col))) > 1 || unique(c(ncol(data.frame(facet_rows)),check_input_col)) != 1)
        stop("invalid arguments: check that the inputs have a single column")

      dat <- data.frame(id = id, y = category, flag1 = groups$flag1, flag2 = groups$flag2, bar_color = block_color, f_rows = facet_rows)

      if(block_count == "# of patients"){
        counts1 <- dat %>% group_by(id, y, flag1, flag2, f_rows) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color)) %>%
          distinct %>% group_by(y, flag1, flag2, f_rows) %>%
          select(-id) %>%
          group_by(y, flag1, f_rows) %>%
          tally %>%
          filter(flag1 == 1) %>%
          as.data.frame()
        counts2 <-  dat %>% group_by(id, y, flag1, flag2, f_rows) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color)) %>%
          distinct %>% group_by(y, flag1, flag2, f_rows) %>%
          select(-id) %>%
          group_by(y, flag2, f_rows) %>%
          tally %>%
          filter(flag2 == 1) %>%
          as.data.frame()
        in_dat <- data.frame(id = id, y = category, flag1 = groups$flag1, flag2 = groups$flag2, bar_color = block_color, f_rows = facet_rows)
        temp1 <- in_dat %>% select(-flag2) %>%
          group_by(id, y, flag1, f_rows) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color))%>% distinct %>% as.data.frame()
        temp2 <- in_dat %>% select(-flag1) %>%
          group_by(id, y, flag2, f_rows) %>%
          arrange(bar_color) %>%
          filter(bar_color == last(bar_color))%>% distinct %>% as.data.frame()
        temp1 <- temp1[,-1]
        temp2 <- temp2[,-1]
      } else if(block_count == "# of AEs"){
        counts1 <- dat %>% group_by(y, flag1, f_rows) %>% tally %>% as.data.frame()
        counts2 <- dat %>% group_by(y, flag2, f_rows) %>% tally %>% as.data.frame()
        temp1 <- data.frame(id = id, y = category, flag1 = groups$flag1, bar_color = block_color, f_rows = facet_rows)
        temp2 <- data.frame(id = id, y = category, flag2 = groups$flag2, bar_color = block_color, f_rows = facet_rows)
        temp1 <- temp1[,-1]
        temp2 <- temp2[,-1]
      }

      temp1$bar_color <- factor(temp1$bar_color)
      temp2$bar_color <- factor(temp2$bar_color)
      counts1 <- left_join(counts1, temp1, by = c("y", "flag1", "f_rows"))
      counts2 <- left_join(counts2, temp2, by = c("y", "flag2", "f_rows"))
      max_c <- max(c(counts1$n, counts2$n))

      counts1$n0 <- rep(1, nrow(counts1))
      counts1 <- counts1 %>% arrange(desc(bar_color))
      counts1 <- ddply(counts1, c("y", "flag1", "f_rows"), transform, label_ypos=cumsum(n0))
      counts1 <- ddply(counts1, c("y", "flag1",  "bar_color", "f_rows"), transform, bar_count=sum(n0))
      text_ann1 <- counts1 %>% arrange(bar_color) %>%
        group_by(y, flag1, bar_color, f_rows) %>%
        filter(label_ypos == max(label_ypos))
      total_text_ann1 <- text_ann1 %>% group_by(y, flag1, f_rows) %>% filter(label_ypos == max(label_ypos))

      counts2$n0 <- rep(1, nrow(counts2))
      counts2 <- counts2 %>% arrange(desc(bar_color))
      counts2 <- ddply(counts2, c("y", "flag2", "f_rows"), transform, label_ypos=cumsum(n0))
      counts2 <- ddply(counts2, c("y", "flag2",  "bar_color", "f_rows"), transform, bar_count=sum(n0))
      text_ann2 <- counts2 %>% arrange(bar_color) %>%
        group_by(y, flag2, bar_color, f_rows) %>%
        filter(label_ypos == max(label_ypos))
      total_text_ann2 <- text_ann2 %>% group_by(y, flag2, f_rows) %>% filter(label_ypos == max(label_ypos))

    }
  }else{
    if(is.null(facet_rows)){
      dat <- data.frame(id = id, y = category, flag1 = groups$flag1, flag2 = groups$flag2)

      if(block_count == "# of patients"){
        counts1 <- dat %>% group_by(id, y, flag1, flag2) %>%
          distinct %>% group_by(y, flag1, flag2) %>%
          select(-id) %>%
          group_by(y, flag1) %>%
          tally %>%
          filter(flag1 == 1) %>%
          as.data.frame()
        counts2 <-  dat %>% group_by(id, y, flag1, flag2) %>%
          distinct %>% group_by(y, flag1, flag2) %>%
          select(-id) %>%
          group_by(y, flag2) %>%
          tally %>%
          filter(flag2 == 1) %>%
          as.data.frame()
        in_dat <- data.frame(id = id, y = category, flag1 = groups$flag1, flag2 = groups$flag2)
        temp1 <- in_dat %>% select(-flag2) %>%
          group_by(id, y, flag1) %>%
          distinct %>% as.data.frame()
        temp2 <- in_dat %>% select(-flag1) %>%
          group_by(id, y, flag2) %>%
          distinct %>% as.data.frame()
        temp1 <- temp1[,-1]
        temp2 <- temp2[,-1]
      } else if(block_count == "# of AEs"){
        counts1 <- dat %>% group_by(y, flag1) %>% tally %>% as.data.frame()
        counts2 <- dat %>% group_by(y, flag2) %>% tally %>% as.data.frame()
        temp1 <- data.frame(id = id, y = category, flag1 = groups$flag1)
        temp2 <- data.frame(id = id, y = category, flag2 = groups$flag2)
        temp1 <- temp1[,-1]
        temp2 <- temp2[,-1]
      }

      counts1 <- left_join(counts1, temp1, by = c("y", "flag1"))
      counts2 <- left_join(counts2, temp2, by = c("y", "flag2"))
      max_c <- max(c(counts1$n, counts2$n))

      counts1$n0 <- rep(1, nrow(counts1))
      counts1 <- ddply(counts1, c("y", "flag1"), transform, label_ypos=cumsum(n0))
      total_text_ann1 <- text_ann1 %>% group_by(y, flag1) %>% filter(label_ypos == max(label_ypos))

      counts2$n0 <- rep(1, nrow(counts2))
      counts2 <- ddply(counts2, c("y", "flag2"), transform, label_ypos=cumsum(n0))
      total_text_ann2 <- text_ann2 %>% group_by(y, flag2) %>% filter(label_ypos == max(label_ypos))

    } else{
      if(length(unique(c(nrow(data.frame(facet_rows)),check_input_length))) > 1)
        stop("invalid arguments: check that the length of input arguments are identical")
      if(length(unique(c(ncol(data.frame(facet_rows)),check_input_col))) > 1 || unique(c(ncol(data.frame(facet_rows)),check_input_col)) != 1)
        stop("invalid arguments: check that the inputs have a single column")

      dat <- data.frame(id = id, y = category, flag1 = groups$flag1, flag2 = groups$flag2, f_rows = facet_rows)

      if(block_count == "# of patients"){
        counts1 <- dat %>% group_by(id, y, flag1, flag2, f_rows) %>%
          distinct %>% group_by(y, flag1, flag2, f_rows) %>%
          select(-id) %>%
          group_by(y, flag1, f_rows) %>%
          tally %>%
          filter(flag1 == 1) %>%
          as.data.frame()
        counts2 <-  dat %>% group_by(id, y, flag1, flag2, f_rows) %>%
          distinct %>% group_by(y, flag1, flag2, f_rows) %>%
          select(-id) %>%
          group_by(y, flag2, f_rows) %>%
          tally %>%
          filter(flag2 == 1) %>%
          as.data.frame()
        in_dat <- data.frame(id = id, y = category, flag1 = groups$flag1, flag2 = groups$flag, f_rows = facet_rows)
        temp1 <- in_dat %>% select(-flag2) %>%
          group_by(id, y, flag1, f_rows) %>%
          distinct %>% as.data.frame()
        temp2 <- in_dat %>% select(-flag1) %>%
          group_by(id, y, flag2, f_rows) %>%
          distinct %>% as.data.frame()
        temp1 <- temp1[,-1]
        temp2 <- temp2[,-1]
      } else if(block_count == "# of AEs"){
        counts1 <- dat %>% group_by(y, flag1, f_rows) %>% tally %>% as.data.frame()
        counts2 <- dat %>% group_by(y, flag2, f_rows) %>% tally %>% as.data.frame()
        temp1 <- data.frame(id = id, y = category, flag1 = groups$flag1, f_rows = facet_rows)
        temp2 <- data.frame(id = id, y = category, flag2 = groups$flag2, f_rows = facet_rows)
        temp1 <- temp1[,-1]
        temp2 <- temp2[,-1]
      }

      counts1 <- left_join(counts1, temp1, by = c("y", "flag1", "f_rows"))
      counts2 <- left_join(counts2, temp2, by = c("y", "flag2", "f_rows"))
      max_c <- max(c(counts1$n, counts2$n))

      counts1$n0 <- rep(1, nrow(counts1))
      counts1 <- ddply(counts1, c("y", "flag1", "f_rows"), transform, label_ypos=cumsum(n0))
      total_text_ann1 <- text_ann1 %>% group_by(y, flag1, f_rows) %>% filter(label_ypos == max(label_ypos))

      counts2$n0 <- rep(1, nrow(counts2))
      counts2 <- ddply(counts2, c("y", "flag2", "f_rows"), transform, label_ypos=cumsum(n0))
      total_text_ann2 <- text_ann2 %>% group_by(y, flag2, f_rows) %>% filter(label_ypos == max(label_ypos))
    }
  }

  counts1$y <- str_wrap(counts1$y, width = 30)
  total_text_ann1$y <- str_wrap(total_text_ann1$y, width = 30)
  text_ann1$y <- str_wrap(text_ann1$y, width = 30)

  counts2$y <- str_wrap(counts2$y, width = 30)
  total_text_ann2$y <- str_wrap(total_text_ann2$y, width = 30)
  text_ann2$y <- str_wrap(text_ann2$y, width = 30)

  if(sort_by == "alphabetical"){
    counts <- data.frame(y = c(as.character(counts1$y), as.character(counts2$y)))
    counts1$y <- factor(counts1$y, levels = rev(unique(counts$y[order(counts$y, decreasing = TRUE)])))
    counts2$y <- factor(counts2$y, levels = rev(unique(counts$y[order(counts$y, decreasing = TRUE)])))

  } else if(sort_by == "count"){
    tot1 <- counts1 %>% group_by(y, flag1) %>%
      filter(label_ypos == last(label_ypos)) %>%
      group_by(y) %>%
      tally(n) %>%
      data.frame
    tot2 <- counts2 %>% group_by(y, flag2) %>%
      filter(label_ypos == last(label_ypos)) %>%
      group_by(y) %>%
      tally(n) %>%
      data.frame
    tot <- rbind(tot1, tot2) %>% group_by(y) %>% tally(nn) %>% as.data.frame

    counts1$y <- factor(counts1$y, levels = rev(unique(tot$y[order(tot[, 2], decreasing = TRUE)])))
    counts2$y <- factor(counts2$y, levels = rev(unique(tot$y[order(tot[, 2], decreasing = TRUE)])))

  }

  if(is.null(group_names)){
    g1 <- names(rightFlag)[1]
    g2 <- names(leftFlag)[2]
  } else{
    g1 <- group_names[1]
    g2 <- group_names[2]
  }

  #plot butterfly plot --------------------
  if(!is.null(block_color)){
    pl <- ggplot(NULL, aes(x=y)) +
      geom_bar(data=counts1, aes(y=n0, fill=bar_color), stat="identity") +
      geom_bar(data=counts2, aes(y=-n0, fill=bar_color), stat="identity") +
      geom_hline(yintercept=0, colour="black", lwd=0.4) +
      geom_text(data=text_ann1, aes(y = label_ypos - 0.2, label = bar_count), hjust=1) +
      geom_text(data=text_ann2, aes(y = -label_ypos, label = bar_count), hjust=-0.9) +
      geom_text(data=total_text_ann1, aes(y = label_ypos, label = n), fontface = "bold",hjust=-1) +
      geom_text(data=total_text_ann2, aes(y = -label_ypos - 0.4, label = n), fontface = "bold",hjust=0.9) +
      coord_flip() +
      scale_y_continuous(labels = abs, limits = (max_c*1.2) * c(-1,1)) +
      labs(x = y_label, y = block_count, fill = legend_label)
  } else {
    pl <- ggplot(NULL, aes(x=y)) +
      geom_bar(data=counts1, aes(y=n0, fill=bar_color), stat="identity") +
      geom_bar(data=counts2, aes(y=-n0, fill=bar_color), stat="identity") +
      geom_hline(yintercept=0, colour="black", lwd=0.4) +
      geom_text(data=total_text_ann1, aes(y = label_ypos, label = n), fontface = "bold",hjust=-1) +
      geom_text(data=total_text_ann2, aes(y = -label_ypos - 0.4, label = n), fontface = "bold", hjust=0.9) +
      coord_flip() +
      scale_y_continuous(labels = abs, limits = (max_c*1.2) * c(-1,1)) +
      labs(x = y_label, y = block_count, fill = legend_label)
  }

  if(!is.null(facet_rows)){
    pl <- pl + facet_wrap(~f_rows, ncol = 1)
  }

  if(show_legend){
    pl <- pl + theme_bw() +
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
    pl <- pl + theme_bw() +
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
    pl <- pl + scale_x_discrete(limits = rev(unique(tot$y[order(tot[, 2], decreasing = TRUE)])))
  }

  pl <- pl + labs(title = str_wrap(g2, width = 30))
  g <- ggplotGrob(pl)

  g2 <- gtable_add_grob(g, textGrob(str_wrap(g1, width = 30), x=1, just = "right", hjust=1, gp=gpar(fontsize = 11)),
                        t=2, l=4, b=2, r=4, name="right-title")
  grid.draw(g2)

}


