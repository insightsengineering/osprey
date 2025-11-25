# Decorate grob (`gTree`) objects then outputs as `IDM` compatible PDF

This is an utility function to decorated grob (`gTree`) object with
titles and footnotes in accordance with `IDM` specification and export
as PDF file with full path to program and the output for easy tracking
and archiving.

## Usage

``` r
grobs2pdf(
  grobs,
  titles,
  footnotes,
  progpath,
  outpath,
  fontsize = 9,
  pagesize = "letter.landscape"
)
```

## Arguments

- grobs:

  A grid grob (`gTree`) object, optionally `NULL` if only a grob with
  the decoration should be shown

- titles:

  Vector of character strings. Vector elements are separated by a
  newline and strings are wrapped according to the page with

- footnotes:

  Vector of character string. Same rules as for `titles`

- progpath:

  Specify the full path to the R program that generate the grobs and the
  PDF

- outpath:

  Specify full path to output pdf to `BCE` or `BEE`

- fontsize:

  Base font size used in pdf, default set to 9. Font size for title is
  set to `fontsize` + 1 (default = 10) and for footnotes set to
  `fontsize` - 1 (default = 8)

- pagesize:

  name of paper size and orientation, accepted values include
  `"a4.landscape"`, `"a4.portrait"`, `"letter.portrait"` and
  `"letter.landscape"` (default)

## Value

a pdf file

## Author

Chendi Liao (liaoc10) <chendi.liao@roche.com>

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

g <- list(
  ggplotGrob(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point()
  ),
  ggplotGrob(
    ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
      geom_point()
  ),
  ggplotGrob(
    ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, color = Species)) +
      geom_point()
  )
)

grobs2pdf(
  grobs = g,
  titles = "Visualization of Iris Data",
  footnotes = "This is a footnote",
  progpath = "~/example_prog.R",
  outpath = "~/example_grobs2pdf.pdf"
)
} # }
```
