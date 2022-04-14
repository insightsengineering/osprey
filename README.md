
# osprey

`osprey` is an R package for community contributed analysis functions to create TLGs (tables, listing and graphs) for analysis of clinical trial data. These functions be used stand alone to generate static analysis, but are also called by the [teal.osprey](https://github.com/insightsengineering/teal.osprey) package which provides `teal` modules to be used inside `teal` applications.

The package provides:
<!-- markdownlint-disable MD007 MD030 -->
-   waterfall plots (`g_waterfall`)
-   swim lane plots (`g_swimlane`)
-   spider plots (`g_spiderplot`)
-   butterfly plots (`g_butterfly`)
-   and many more
<!-- markdownlint-enable MD007 MD030 -->
## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/osprey@*release")
```

In order to run many of the examples you will also need to install the [`scda`](https://github.com/insightsengineering/scda) package.
