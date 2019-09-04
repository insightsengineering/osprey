#' Load Multiple SAS datasets from BCE
#'
#' This is an utility function to quickly read-in one or multiple SAS datasets
#' from BCE without typing out the full path, while keeping the file name as the
#' assigned dataset name.
#'
#' @param snapshot name of the BCE snapshot in string format (eg. "s12345a")
#' @param area specify the BCE area in string format, accepted values are
#'   \code{"sdtm"}, \code{"prod"}, \code{"qa"} and \code{"home"}
#' @param x a character vector containing the name of the SAS datasets to be
#'   loaded. Default is \code{NULL}, where all datasets within the snapshot
#'   directory will be loaded.
#' @param load if \code{TRUE} (default), all read-in datasets will be loaded.
#'   Use \code{FALSE} if do not want to load datasets automatically, and assign
#'   to an object.
#'
#' @return dataframe objects
#'
#' @import rocheBCE
#' @export
#'
#' @author Chendi Liao (liaoc10) \email{chendi.liao@@roche.com}
#'
#' @examples
#'
#' \dontrun{
#' # Example 1 - load add datasets in directory
#' getdata_bce("s39148b", "prod")
#'
#' # Example 2 - load only specified datasets
#' getdata_bce("s39148b", "sdtm", c("dm", "ex", "ds"))
#'
#' # Example 3 - load from home
#' # equivalent to read_bce('/opt/BIOSTAT/home/<UNIXID>/<snapshot>/libraries/asl.sas7bdat')
#' snapshot <- "s39148b"
#' getdata_bce(snapshot, "home", "asl")
#' }
#'
getdata_bce <- function(snapshot, area, x = NULL, load = TRUE) {

  # If no datasets specified, load all datasets in directory
  if (is.null(x)) {
    dflist <- NULL
    dfname <- NULL
  } else {
    dflist <- as.list(x)
    dfname <- paste0(dflist, ".sas7bdat")
  }

  snapshot <- tolower(snapshot)
  area <- tolower(area)

  # Set up paths
  path <- if (area == "sdtm") {
    paste("/opt/BIOSTAT", area, snapshot, dfname, sep = "/")
  } else if (area %in% c("qa", "prod")) {
    paste("/opt/BIOSTAT", area, snapshot, "libraries", dfname, sep = "/")
  } else if (area == "home") {
    paste("/opt/BIOSTAT", area, Sys.getenv("LOGNAME"), snapshot, "libraries", dfname, sep = "/")
  } else {
    stop("Please specify a valid snapshot area such as qa, prod, home or sdtm")
  }


  if (is.null(x)) {
    path <- gsub(".$", "", path)
    data <- read_bce(path)
  } else {
    data <- lapply(path, read_bce)
    names(data) <- dflist
  }

  if (isTRUE(load)) {
    invisible(list2env(data, envir = .GlobalEnv))
  } else {
    return(data)
  }
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
    paper_height <<- 11.7
  } else if (pagesize == "letter.portrait") {
    paper_width <- 8.5
    paper_height <<- 11
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
      x = unit(left_margin, "inches"), y = unit(bottom_margin, "inches"),
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
