# Extract specific part of a `ggplot` or grob

Extract specific part of a `ggplot` or grob

## Usage

``` r
grob_part(gplot_grob, part)
```

## Arguments

- gplot_grob:

  `ggplot` or grob object

- part:

  name of the part to be extracted. `NA` will return
  [`zeroGrob()`](https://ggplot2.tidyverse.org/reference/zeroGrob.html)
