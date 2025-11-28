# Output decorated grob (`gTree`) objects as PDF

This is an utility function to output a decorated grob (`gTree`) object

## Usage

``` r
as_pdf(grobs, outpath, pagesize = "letter.landscape")
```

## Arguments

- grobs:

  a grid grob (`gTree`) object, optionally `NULL` if only a grob with
  the decoration should be shown.

- outpath:

  specify full path to output pdf to `BCE` or `BEE`

- pagesize:

  name of `pagesize` (print size) and orientation, accepted values
  include `"a4.landscape"`, `"a4.portrait"`, `"letter.portrait"` and
  `"letter.landscape"` (default)

## Value

a pdf file

## See also

[`grobs2pdf()`](https://insightsengineering.github.io/osprey/reference/grobs2pdf.md)

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

# output to pdf
as_pdf(g, "~/example_aspdf1.pdf")
} # }
```
