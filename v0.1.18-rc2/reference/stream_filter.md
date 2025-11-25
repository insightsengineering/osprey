# Applies STREAM style filtering to datasets

One of `slref` or `anl` need to be specified. The conversion from `SAS`
code in filters dataset may not work in all cases. In case of failure a
sensible error message should be returned.

## Usage

``` r
stream_filter(
  slref = NULL,
  anl = NULL,
  filters,
  suffix,
  slref_keep = NULL,
  usubjid = "USUBJID"
)
```

## Arguments

- slref:

  The subject level data frame (typically `ADSL`)

- anl:

  The analysis data frame

- filters:

  The name of the filters dataset

- suffix:

  The suffix to apply in quotes (e.g. `"ITT_PFSINV"`)

- slref_keep:

  Variables to keep from `slref` (e.g. `c("REGION", "SEX")`)

- usubjid:

  The unique subject identifier variable in quotes (e.g. `"USUBJID"`)

## Value

`dataframe` object

## Author

Iain Bennett

## Examples

``` r
ADSL <- osprey::rADSL
ADTTE <- osprey::rADTTE
filters <- as.data.frame(rbind(
  c(ID = "IT", FLTTARGET = "SLREF", FLTWHERE = "where 1 eq 1"),
  c(ID = "BIO", FLTTARGET = "SLREF", FLTWHERE = "where BMRKR1 ge 4.3"),
  c(ID = "M", FLTTARGET = "SLREF", FLTWHERE = "where SEX eq 'M'"),
  c(ID = "PFS", FLTTARGET = "ANL", FLTWHERE = "where PARAMCD eq 'PFS'"),
  c(ID = "OS", FLTTARGET = "ANL", FLTWHERE = "where PARAMCD eq 'OS'")
))

ANL <- stream_filter(
  slref = ADSL,
  anl = ADTTE,
  suffix = "IT_PFS_BIO",
  filters = filters
)
#> 
#> Filter IT applied 
#> SAS code: where 1 eq 1 
#> was converted to
#> R code:   1 == 1  
#> 400 of 400 observations selected from SLREF 
#> 
#> Filter PFS applied 
#> SAS code: where PARAMCD eq 'PFS' 
#> was converted to
#> R code:   PARAMCD == 'PFS'  
#> 400 of 2000 observations selected from ANL 
#> 
#> Filter BIO applied 
#> SAS code: where BMRKR1 ge 4.3 
#> was converted to
#> R code:   BMRKR1 >= 4.3  
#> 231 of 400 observations selected from SLREF 
#> 
#> Suffix IT_PFS_BIO was applied
#> 231 of 2000 observations selected from ANL
```
