# Replicates the use of index function in `SAS` for logic options

Assumption is that use in filters is to only resolve true vs false
Primarily for use with stream_filter and related
`stream_filter_convwhere` functions

## Usage

``` r
stream_filter_index(string1, string2)
```

## Arguments

- string1:

  The string to search within - can be a vector

- string2:

  The string to search for - must have length 1

## Value

`boolean` indicator

## Author

Iain Bennett

## Examples

``` r
AEACN <- c("DRUG MODIFIED", "DRUG STOPPED", "DOSE/DRUG MODIFIED")
stream_filter_index(AEACN, "DRUG MODIFIED")
#> [1]  TRUE FALSE  TRUE
```
