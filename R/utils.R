#' Output decorated grob (gTree) objects as PDF
#'
#' This is an utility function to output a decorated grob (gTree) object
#'
#' @param grobs a grid grob (gTree) object, optionally `NULL` if only a grob with
#'   the decoration should be shown.
#' @param outpath specify full path to output pdf to BCE or BEE
#' @param pagesize name of pagesize (print size) and orientation, accepted values include
#'   \code{"a4.landscape"}, \code{"a4.portrait"}, \code{"letter.portrait"} and
#'   \code{"letter.landscape"} (default)
#'
#' @return a pdf file
#' @seealso [grobs2pdf()]
#' @export
#'
#' @author Chendi Liao (liaoc10) \email{chendi.liao@roche.com}
#'
#' @examples
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
#' }
as_pdf <- function(grobs,
                   outpath,
                   pagesize = "letter.landscape") {
  paper_sizes <- paper_size(pagesize)
  paper_width <- paper_sizes[1]
  paper_height <- paper_sizes[2]

  # Output to PDF
  grDevices::pdf(outpath, width = paper_width, height = paper_height)

  lapply(grobs, function(x) {
    grid::grid.newpage()
    grid::grid.draw(x)
  })

  grDevices::dev.off()
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
#' @param grobs A grid grob (gTree) object, optionally `NULL` if only a
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
#' @author Chendi Liao (liaoc10) \email{chendi.liao@roche.com}
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(tern)
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
#'
grobs2pdf <- function(grobs,
                      titles,
                      footnotes,
                      progpath,
                      outpath,
                      fontsize = 9,
                      pagesize = "letter.landscape") {
  if (!requireNamespace("tern", quietly = TRUE)) {
    stop("This function requires the R package tern to be available - please install the package.")
  }

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
  dg <- tern::decorate_grob_set(
    grobs = grobs,
    titles = titles,
    footnotes = paste(footnotes, logtext, sep = "\n\n"),
    # outer_margins = grid::unit(c(bottom.margin, left.margin, top.margin, right.margin), "inches"),
    outer_margins = grid::unit(c(0, 0, 0, 0), "lines"),
    padding = grid::unit(0.5, "lines"),
    gp_titles = grid::gpar(fontsize = fontsize + 1, fontface = 2, lineheight = 1),
    gp_footnotes = grid::gpar(fontsize = fontsize - 1, fontface = 1, lineheight = 1),
    gp = grid::gpar(fontsize = fontsize),
    vp = grid::viewport(
      x = grid::unit(left_margin, "inches"),
      y = grid::unit(bottom_margin, "inches"),
      width = grid::unit(paper_width - left_margin - right_margin, "inches"),
      height = grid::unit(paper_height - top_margin - bottom_margin, "inches"),
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
#'
grob_part <- function(gplot_grob, part) {
  if (is.na(part)) {
    return(zeroGrob())
  }
  stopifnot(length(part) == 1 && is.character(part))
  index <- match(part, gplot_grob$layout$name)
  if (is.na(index)) {
    stop(c(
      part, " not in plot object. Allowed parts are ",
      paste(gplot_grob$layout$name, collapse = ", ")
    ))
  }
  grob <- gplot_grob$grobs[[index]]
  return(grob)
}

#' Add padding to grob
#' @param grob grob object
#' @param pad_v padding to add vertically
#' @param pad_h padding to add horizontally
#' @keywords internal
#'
grob_add_padding <- function(grob, pad_v = grid::unit(5, "pt"), pad_h = grid::unit(5, "pt")) {
  ret <- gtable::gtable(
    heights = grid::unit.c(pad_v, grid::unit(1, "null"), pad_v),
    widths = grid::unit.c(pad_h, grid::unit(1, "null"), pad_h)
  )
  # t, b, l, r, z arguments do not need modification
  # same effect can be achieved by modifying pad_v and pad_h
  ret <- gtable::gtable_add_grob(ret, grob, t = 2, b = 2, l = 2, r = 2, z = 1, name = "panel")
  ret <- gtable::gtable_add_grob(ret, grid::rectGrob(), t = 1, b = 3, l = 1, r = 3, z = 0, name = "background")
  return(ret)
}

#' this theme is used across many figures. can be safely removed if update the theme in each function
#' @param axis_side axis position
#' @param fontsize font size in 'mm'
#' @keywords internal
#'
theme_osprey <- function(axis_side = "left", fontsize = 4) {
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
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
    plot.title = element_text(hjust = 0.5)
  )
}

check_same_N <- function(..., omit_null = TRUE) { # nolint
  dots <- list(...)

  n_list <- Map(
    function(x, name) {
      if (is.null(x)) {
        if (omit_null) {
          NA_integer_
        } else {
          stop("arg", name, "is not supposed to be NULL")
        }
      } else if (is.data.frame(x)) {
        nrow(x)
      } else if (is.atomic(x)) {
        length(x)
      } else {
        stop("data structure for ", name, "is currently not supported")
      }
    },
    dots, names(dots)
  )

  n <- stats::na.omit(unlist(n_list))

  if (length(unique(n)) > 1) {
    sel <- which(n != n[1])
    stop("dimension mismatch:", paste(names(n)[sel], collapse = ", "), " do not have N=", n[1])
  }

  TRUE
}

to_n <- function(x, n) {
  if (is.null(x)) {
    NULL
  } else if (length(x) == 1) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop("dimension mismatch")
  }
}

#' Extract specific part of a ggplot or grob
#'
#' @param gplot_grob ggplot or grob object
#' @param part name of the part to be extracted. NA will return zeroGrob()
#' @keywords internal
#'
grob_part <- function(gplot_grob, part) {
  if (is.na(part)) {
    return(zeroGrob())
  }
  stopifnot(length(part) == 1 && is.character(part))
  index <- match(part, gplot_grob$layout$name)
  if (is.na(index)) {
    stop(c(
      part, " not in plot object. Allowed parts are ",
      paste(gplot_grob$layout$name, collapse = ", ")
    ))
  }
  grob <- gplot_grob$grobs[[index]]
  return(grob)
}

#' Extract specific parts of a ggplot or grob
#'
#' @param gplot ggplot or grob object
#' @param parts names vector of the parts to be extracted.
#' @keywords internal
#'
grob_parts <- function(gplot, parts) {
  stopifnot("gplot must inherit from class 'ggplot' or 'grob'" = inherits(gplot, c("ggplot", "grob")))

  if ("ggplot" %in% class(gplot)) {
    gplot_grob <- ggplotGrob(gplot)
  } else if ("grob" %in% class(gplot)) {
    gplot_grob <- gplot
  }
  ret <- lapply(parts, grob_part, gplot = gplot_grob)
  names(ret) <- parts
  return(ret)
}


#' this theme is used across many figures. can be safely removed if update the theme in each function
#' @param axis_side axis position
#' @param fontsize font size in 'mm'
#' @param blank whether to have blank or background with grids and borders
theme_osprey <- function(axis_side = "left", fontsize = 4, blank = FALSE) {
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major.y = if (blank) element_blank() else element_line(colour = "grey50", linetype = 2),
    panel.border = if (blank) element_blank() else element_rect(colour = "black", fill = NA, size = 1),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.ticks.y = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.text = element_text(color = "black", size = fontsize * .pt),
    axis.text.y = element_text(hjust = ifelse(axis_side == "left", 0, 1)),
    text = element_text(size = fontsize * .pt, face = "bold", color = "black"),
    legend.text = element_text(size = fontsize * .pt),
    plot.title = element_text(hjust = 0.5)
  )
}
