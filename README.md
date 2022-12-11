

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering//actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering//actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering//actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io//)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering//main?color=purple\&label=package%20version)](https://github.com/insightsengineering//tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/?color=red\&label=open%20issues)](https://github.com/insightsengineering//issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->


# osprey

<!-- start badges -->
[![Code Coverage](https://raw.githubusercontent.com/insightsengineering/osprey/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/osprey/_xml_coverage_reports/data/main/coverage.xml)
<!-- end badges -->

`osprey` is an R package for community contributed analysis functions to create TLGs (tables, listing and graphs) for the analysis of clinical trials data. These functions can be used stand alone to generate static analysis, but are also called by the [teal.osprey](https://insightsengineering.github.io/teal.osprey/) package which provides `teal` modules to be used inside `teal` applications.

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

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/osprey.svg)](https://starchart.cc/insightsengineering/osprey)

### Stargazers

[![Stargazers repo roster for @insightsengineering/osprey](https://reporoster.com/stars/insightsengineering/osprey)](https://github.com/insightsengineering/osprey/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/osprey](https://reporoster.com/forks/insightsengineering/osprey)](https://github.com/insightsengineering/osprey/network/members)
