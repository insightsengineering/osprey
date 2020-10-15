# tabulation function for condition checks
#' @importFrom utils getFromNamespace
#' @importFrom rtables rrowl rheader header<- header rcell by_all rtabulate
t_helper_tabulate <- function(df_id, n, checkcol, term, remove_dupl, with_percent) { # nolint
  if (checkcol == "rowcount") {
    tbl <- rtabulate(
      na.omit(df_id),
      row_by = by_all(""),
      col_by = df_id$col_by,
      FUN = nrow,
      format = "xx"
    )
  } else if (checkcol == "uniqueid") {
    if (remove_dupl) {
      df_id <- df_id[!duplicated(df_id$id), ]
    }

    if (with_percent) {
      tbl <- rtabulate(
        na.omit(df_id),
        row_by = by_all(""),
        col_by = df_id$col_by,
        FUN = count_perc_col_N,
        col_wise_args = list(n_i = n),
        format = "xx (xx.xx%)"
      )
    } else {
      tbl <- rtabulate(
        na.omit(df_id),
        row_by = by_all(""),
        col_by = df_id$col_by,
        FUN = count_col_N,
        col_wise_args = list(n_i = n),
        format = "xx"
      )
    }
  } else {
    if (remove_dupl) {
      # sort by checkcol in descending order first
      df_id <- df_id[order(df_id[checkcol], decreasing = TRUE), ]
      df_id <- df_id[!duplicated(df_id$id), ]
    }

    tbl <- rtabulate(
      na.omit(df_id),
      row_by = as.factor(df_id[[checkcol]]),
      col_by = df_id$col_by,
      FUN = count_perc_col_N,
      col_wise_args = list(n_i = n),
      format = "xx (xx.xx%)"
    )

    if (dim(tbl)[1] > 1 | (dim(tbl)[1] == 1 & attributes(tbl[1])$names == "1")) {
      tbl <- tbl[dim(tbl)[1]]
    } else {
      for (i in 1:dim(tbl)[2]) {
        tbl[[1]][[i]] <- rcell(0)
      }
    }
  }

  attr(tbl[[1]], "row.name") <- term

  header(tbl) <- rheader(
    rrowl("", levels(df_id$col_by)),
    rrowl("", unname(n), format = "(N=xx)")
  )
  tbl
}

# checks if there is any case and derives counts, otherwise 0
count_col_N <- function(x_cell, n_i) { # nolint
  if (n_i > 0) {
    length(x_cell$id) # obtaining the total
  } else {
    rcell(0, format = "xx")
  }
}


# adds row name to rtable
shift_label_table_no_grade <- function(tbl, term) {
  attr(tbl[[1]], "row.name") <- term
  tbl
}

# shifts labels - used only in t_ae_ctc_v2
shift_label_table_t_ae_ctc_v2 <- function(tbl, term) {
  attr(tbl[[1]], "row.name") <- term
  tbl
}

# remove null elements from list
remove_null <- function(x) {
  x <- Filter(Negate(is.null), x)
  lapply(x, function(x) {
    if (is.list(x) && class(x) != "rtable") remove_null(x) else x
  })
}

# recursive indent function
recursive_indent <- function(tbl_l, ind_count) {
  if (class(tbl_l) == "rtable") {
    in_t <- list(" " = tbl_l)
    t <- do.call(stack_rtables_condense, in_t)
    for (i in 1:nrow(t)) {
      attr(t[[i]], "indent") <- attr(t[[i]], "indent") + ind_count
    }
    t
  } else if (is.list(tbl_l) && class(tbl_l) != "rtable") {
    count <- lapply(tbl_l, function(x) {
      if (class(x) == "rtable") {
        ind_count
      } else {
        ind_count + 1
      }
    })
    count <- unlist(count)
    t0 <- Map(recursive_indent, tbl_l, count)
    tbl <- do.call(stack_rtables_condense, t0) # nolint
  }
}

# arguments for total in tables (AET01, AET02, DST01)
tot_column <- function(choice = c("All Patients")) {
  choice <- match.arg(choice)
  return(choice)
}

#' Stack rtables
#'
#' @param ... rtable objects
#'
#' @return rtable object
#' @noRd
#'
stack_rtables <- function(...) {
  rbind(..., gap = 1)
}

#' Stack rtables with rbind
#'
#' @param ... rtable objects
#' @param nrow_pad number of empty rows between tables in \code{...}
#' @importFrom methods is
#' @noRd
#'
stack_rtables_condense <- function(..., nrow_pad = 1) {
  tbls <- Filter(Negate(is.null), list(...))

  if (length(tbls) > 0) {

    if (!all(vapply(tbls, is, logical(1), "rtable"))) {
      stop("not all objects are of type rtable")
    }

    Reduce(
      function(x, y) rbind(x, y),
      tbls
    )
  } else {
    list()
  }
}

#' Add Adverse Events class
#'
#' @param tbl (\code{tibble}) Containing the data
#' @param class (\code{character}) Class of adverse events to be added as an \code{rtable} row
#'
#' @importFrom rtables rtable
#' @export
add_ae_class <- function(tbl, class) {
  rbind(
    rtable(header(tbl), rrow(class)),
    tbl
  )
}

#' stack a modified version of a data frame
#'
#' essenially rbind(X,modified(X)). this is useful for example when a total
#' column is needed.
#'
#' @param X a data.frame
#' @param ... key=value pairs, where the key refers to a variable in X and value
#'   is the valueof the variable in modified(X)
#'
#' @noRd
#'
#' @importFrom rtables var_labels var_labels<-
#'
#' @examples
#'
#' duplicate_with_var(iris, Species = "Total")
duplicate_with_var <- function(x, ...) { # nolint # nousage
  dots <- list(...)
  nms <- names(dots)
  if (length(nms) > 1 && (is.null(nms) || !all(nms %in% names(x)))) {
    stop("not all names in ... are existent or in X")
  }
  x_copy <- x
  vl <- var_labels(x)
  for (var in nms) {
    x_copy[[var]] <- dots[[var]]
  }
  y <- rbind(x, x_copy)
  var_labels(y) <- vl
  y
}


#' Output decorated grob (gTree) objects as PDF
#'
#' This is an utility function to output a decorated grob (gTree) object
#'
#' @param grobs a grid grob (gTree) object, optionally \code{NULL} if only a grob with
#'   the decoration should be shown.
#' @param outpath specify full path to output pdf to BCE or BEE
#' @param pagesize name of pagesize (print size) and orientation, accepted values include
#'   \code{"a4.landscape"}, \code{"a4.portrait"}, \code{"letter.portrait"} and
#'   \code{"letter.landscape"} (default)
#'
#' @return a pdf file
#'
#' @importFrom grid grid.newpage grid.draw
#'
#' @export
#'
#' @author Chendi Liao (liaoc10) \email{chendi.liao@roche.com}
#'
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#' g <- with(iris, {
#'   list(
#'     ggplotGrob(qplot(Sepal.Length, Sepal.Width, col = Species)),
#'     ggplotGrob(qplot(Sepal.Length, Petal.Length, col = Species)),
#'     ggplotGrob(qplot(Sepal.Length, Petal.Width, col = Species))
#'   )
#' })
#'
#' # output to pdf
#' g %>% as_pdf("~/example_aspdf1.pdf")
#' decorate_grob_set(grobs = g, titles = "Hello\nOne\ntwo", footnotes = "This is a footnote") %>%
#'   as_pdf("~/example_aspdf2.pdf")
#' }
as_pdf <- function(grobs,
                   outpath,
                   pagesize = "letter.landscape") {

  paper_sizes <- paper_size(pagesize)
  paper_width <- paper_sizes[1]
  paper_height <- paper_sizes[2]

  # Output to PDF
  pdf(outpath, width = paper_width, height = paper_height)

  lapply(grobs, function(x) {
    grid.newpage()
    grid.draw(x)
  })

  dev.off()
}


paper_size <- function(pagesize) {
  if (pagesize == "a4.landscape") {
    paper_width <- 11.7
    paper_height <- 8.3
  } else if (pagesize == "a4.portrait") {
    paper_width <- 8.3
    paper_height <- 11.7
  } else if (pagesize == "letter.portrait") {
    paper_width <- 8.5
    paper_height <- 11
  } else if (pagesize == "letter.landscape") {
    paper_width <- 11
    paper_height <- 8.5
  } else {
    paper_width <- 11
    paper_height <- 8.5
  }
  return(c(paper_width, paper_height))
}
#' Decorate grob (gTree) objects then outputs as IDM compatible PDF
#'
#' This is an utility function to decorated grob (gTree) object with titles and
#' footnotes in accordance with IDM specification and export as PDF file with
#' full path to program and the output for easy tracking and archiving.
#'
#' @param grobs A grid grob (gTree) object, optionally \code{NULL} if only a
#'   grob with the decoration should be shown
#' @param titles Vector of character strings. Vector elements are separated by a
#'   newline and strings are wrapped according to the page with
#' @param footnotes Vector of character string. Same rules as for \code{titles}
#' @param progpath Specify the full path to the R program that generate the
#'   grobs and the PDF
#' @param outpath Specify full path to output pdf to BCE or BEE
#' @param fontsize Base font size used in pdf, default set to 9. Font size for
#'   title is set to \code{fontsize} + 1 (default = 10) and for footnotes set to
#'   \code{fontsize} - 1 (default = 8)
#' @param pagesize name of paper size and orientation, accepted values include
#'   \code{"a4.landscape"}, \code{"a4.portrait"}, \code{"letter.portrait"} and
#'   \code{"letter.landscape"} (default)
#'
#' @return a pdf file
#'
#' @export
#'
#' @importFrom tern decorate_grob_set
#' @importFrom grid viewport
#'
#' @author Chendi Liao (liaoc10) \email{chendi.liao@roche.com}
#'
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#'
#' g <- with(iris, {
#'   list(
#'     ggplotGrob(qplot(Sepal.Length, Sepal.Width, col = Species)),
#'     ggplotGrob(qplot(Sepal.Length, Petal.Length, col = Species)),
#'     ggplotGrob(qplot(Sepal.Length, Petal.Width, col = Species))
#'   )
#' })
#'
#' grobs2pdf(
#'   grobs = g,
#'   titles = "Visualization of Iris Data",
#'   footnotes = "This is a footnote",
#'   progpath = "~/example_prog.R",
#'   outpath = "~/example_grobs2pdf.pdf"
#' )
#' }
grobs2pdf <- function(grobs,
                      titles,
                      footnotes,
                      progpath,
                      outpath,
                      fontsize = 9,
                      pagesize = "letter.landscape") {

  # Loads rapid.base.settings list and a few other

  # Page type (default is letter.landscape, options=a4.portrait, a4.landscape, letter.portrait, letter.landscape)
  if (pagesize == "a4.landscape") {
    top_margin <- 1.44
    bottom_margin <- 0.83
    left_margin <- 1.3
    right_margin <- 1.32
  } else if (pagesize == "a4.portrait") {
    top_margin <- 1.32
    bottom_margin <- 1.3
    left_margin <- 1.44
    right_margin <- 0.83
  } else if (pagesize == "letter.portrait") {
    top_margin <- 0.95
    bottom_margin <- 0.98
    left_margin <- 1.5
    right_margin <- 1.0
  } else if (pagesize == "letter.landscape") {
    top_margin <- 1.5
    bottom_margin <- 1.0
    left_margin <- 0.98
    right_margin <- 0.95
  } else {
    top_margin <- 1.5
    bottom_margin <- 1.0
    left_margin <- 0.98
    right_margin <- 0.95
  }

  paper_sizes <- paper_size(pagesize)
  paper_width <- paper_sizes[1]
  paper_height <- paper_sizes[2]

  ## Adding log text to footnotes
  log1 <- paste0("Program: ", progpath, "; Output: ", outpath) # nolint
  log2 <- paste0(format(Sys.time(), "%d%b%Y %H:%M %Z"), ", generated by ", Sys.getenv("USER")) # nolint
  logtext <- paste(mget(ls(pattern = "log")), collapse = "\n")

  ## Make the grobs
  if (class(grobs) != "list") {
    grobs <- list(grobs)
  }

  ## Decorate grobs
  dg <- decorate_grob_set(
    grobs = grobs,
    titles = titles,
    footnotes = paste(footnotes, logtext, sep = "\n\n"),
    # outer_margins = unit(c(bottom.margin, left.margin, top.margin, right.margin), "inches"),
    outer_margins = unit(c(0, 0, 0, 0), "lines"),
    padding = unit(0.5, "lines"),
    gp_titles = gpar(fontsize = fontsize + 1, fontface = 2, lineheight = 1),
    gp_footnotes = gpar(fontsize = fontsize - 1, fontface = 1, lineheight = 1),
    gp = gpar(fontsize = fontsize),
    vp = viewport(
      x = unit(left_margin, "inches"),
      y = unit(bottom_margin, "inches"),
      width = unit(paper_width - left_margin - right_margin, "inches"),
      height = unit(paper_height - top_margin - bottom_margin, "inches"),
      just = c("left", "bottom"),
      name = "OuterMargin"
    )
  )

  # Output as PDF
  as_pdf(
    grobs = dg,
    outpath = outpath,
    pagesize = pagesize
  )
}

#' Extract specific part of a ggplot or grob
#'
#' @param gplot_grob ggplot or grob object
#' @param part name of the part to be extracted. NA will return zeroGrob()
#' @importFrom ggplot2 zeroGrob
#'
grob_part <- function(gplot_grob, part) {
  if (is.na(part)) {
    return(zeroGrob())
  }
  stopifnot(length(part) == 1 & is.character(part))
  index <- match(part, gplot_grob$layout$name)
  if (is.na(index)) {
    stop(c(part, " not in plot object. Allowed parts are ",
           paste(gplot_grob$layout$name, collapse = ", ")))
  }
  grob <- gplot_grob$grobs[[index]]
  return(grob)
}

#' Add padding to grob
#' @param grob grob object
#' @param pad_v padding to add vertically
#' @param pad_h padding to add horizontally
#' @importFrom gtable gtable_add_grob gtable
#' @importFrom grid rectGrob
#'
grob_add_padding <- function(grob, pad_v = unit(5, "pt"), pad_h = unit(5, "pt")) {
  ret <- gtable(heights = unit.c(pad_v, unit(1, "null"), pad_v), widths = unit.c(pad_h, unit(1, "null"), pad_h))
  ret <- gtable_add_grob(ret, grob, t = 2, b = 2, l = 2, r = 2, z = 1, name = "panel")
  ret <- gtable_add_grob(ret, rectGrob(), t = 1, b = 3, l = 1, r = 3, z = 0, name = "background")
  return(ret)
}



#' this theme is used across many figures. can be safely removed if update the theme in each function
#' @importFrom  ggplot2 theme .pt
#' @param axis_side axis position
#' @param fontsize font size in 'mm'
theme_osprey <- function(axis_side = "left", fontsize = 4) {
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major.y = element_line(colour = "grey50", linetype = 2),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text = element_text(color = "black", size = fontsize * .pt),
        axis.text.y = element_text(hjust = ifelse(axis_side == "left", 1, 0)),
        text = element_text(size = fontsize * .pt, face = "bold", color = "black"),
        legend.text = element_text(size = fontsize * .pt),
        plot.title = element_text(hjust = 0.5))
}
