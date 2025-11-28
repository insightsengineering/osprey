# Convert `SAS` code to R code

Will convert following `SAS` operators: `eq, =, le, lt, ge, gt, index`
Will convert following logic: and, or, () Will convert all unquoted
values to upper case (assumed to be variable names) All quoted values
will be returned with single quotes - may fail if have quotes within
quotes

## Usage

``` r
stream_filter_convwhere(x)
```

## Arguments

- x:

  a character string of `SAS` code

## Value

a character string of R code

## Author

Iain Bennett

## Examples

``` r
stream_filter_convwhere(x = "where X in (1 2 3 4) and Y gt 4 ")
#> [1] "  X %in% c(1 , 2 , 3 , 4) & Y > 4  "
stream_filter_convwhere(x = "where X = \"fred\" and Y gt 4 ")
#> [1] "  X == 'fred' & Y > 4  "
```
