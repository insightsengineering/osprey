# Simple spider plot

Description of this plot

## Usage

``` r
spiderplot_simple(
  anl,
  byvar = "USUBJID",
  days = "TRTDURD",
  mes_value = "PARAM",
  group_col = "USUBJID",
  baseday = 0
)
```

## Arguments

- anl:

  The analysis data frame

- byvar:

  Analysis dataset

- days:

  Variable with time in days

- mes_value:

  Variable with measurement

- group_col:

  Variable to color the individual lines and id in plot

- baseday:

  Numeric Value, points with only smaller values will be cut out

## Value

`ggplot` object

## Author

Mika Maekinen

## Examples

``` r
library(dplyr)
library(nestcolor)

ADSL <- osprey::rADSL[1:15, ]
ADTR <- osprey::rADTR
ANL <- left_join(ADSL, ADTR)
#> Joining with `by = join_by(STUDYID, USUBJID, SUBJID, SITEID, AGE, AGEU, SEX,
#> RACE, ETHNIC, COUNTRY, DTHFL, INVID, INVNAM, ARM, ARMCD, ACTARM, ACTARMCD,
#> TRT01P, TRT01A, TRT02P, TRT02A, REGION1, STRATA1, STRATA2, BMRKR1, BMRKR2,
#> ITTFL, SAFFL, BMEASIFL, BEP01FL, AEWITHFL, RANDDT, TRTSDTM, TRTEDTM, TRT01SDTM,
#> TRT01EDTM, TRT02SDTM, TRT02EDTM, AP01SDTM, AP01EDTM, AP02SDTM, AP02EDTM,
#> EOSSTT, EOTSTT, EOSDT, EOSDY, DCSREAS, DTHDT, DTHCAUS, DTHCAT, LDDTHELD,
#> LDDTHGR1, LSTALVDT, DTHADY, ADTHAUT)`

ANL %>%
  dplyr::filter(ANL01FL == "Y" & PARAMCD == "SLDINV") %>%
  spiderplot_simple(group_col = "SEX", days = "ADY", mes_value = "AVAL")
```
