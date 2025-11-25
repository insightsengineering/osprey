# Standard Arguments

The documentation to this function lists all the arguments in `osprey`
that are used repeatedly to express an analysis.

## Arguments

- arm:

  (`factor`)  
  vector that contains arm information in analysis data. For example,
  `ADAE$ACTARMCD`.

- conf_level:

  (`numeric`)  
  the confidence interval level, default is 0.95.

- diff_ci_method:

  (`character`)  
  the method used to calculate confidence interval. Default is `"wald"`.
  Possible choices are methods supported in
  [`BinomDiffCI`](https://andrisignorell.github.io/DescTools/reference/BinomDiffCI.html).

- fontsize:

  (`numeric`)  
  font size for the plot. It is the size used in `ggplot2` with default
  unit "mm", if you want "points" you will need to divide the point
  number by `ggplot2:::.pt`.

- draw:

  (`logical`)  
  whether to draw the plot.

## Details

Although this function just returns `NULL` it has two uses. For the
`osprey` users it provides a documentation of arguments that are
commonly and consistently used in the framework. For the developer it
adds a single reference point to import the `roxygen` argument
description with: `@inheritParams argument_convention`
