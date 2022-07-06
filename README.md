
# osprey

`osprey` is an R package for community contributed analysis functions to create TLGs (tables, listing and graphs) for the analysis of clinical trials data. These functions can be used stand alone to generate static analysis, but are also called by the [teal.osprey](https://insightsengineering.github.io/teal.osprey) package which provides `teal` modules to be used inside `teal` applications.

The package provides:

<!-- markdownlint-disable MD007 MD030 -->
- waterfall plots (`g_waterfall`)
- swim lane plots (`g_swimlane`)
- spider plots (`g_spiderplot`)
- butterfly plots (`g_butterfly`)
- and many more
<!-- markdownlint-enable MD007 MD030 -->

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/osprey@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

In order to run many of the examples you will also need to install the [`scda`](https://github.com/insightsengineering/scda) package.
