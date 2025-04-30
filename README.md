# osprey

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/osprey/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/osprey/)
[![Docs 📚](https://github.com/insightsengineering/osprey/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/osprey/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/osprey/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/osprey/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/osprey?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/osprey?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/osprey)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/osprey)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/osprey)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/osprey)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/osprey)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/osprey)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/osprey/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/osprey/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/osprey?color=red\&label=open%20issues)](https://github.com/insightsengineering/osprey/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

`osprey` is an R package for community contributed analysis functions to create TLGs (tables, listing and graphs) for the analysis of clinical trials data. These functions can be used stand alone to generate static analysis, but are also called by the [teal.osprey](https://insightsengineering.github.io/teal.osprey/) package which provides modules to be used inside [`teal`](https://insightsengineering.github.io/teal/) applications.

The package provides:

<!-- markdownlint-disable MD007 MD030 -->
- waterfall plots ([`g_waterfall`](https://insightsengineering.github.io/osprey/latest-tag/reference/g_waterfall.html))
- swim lane plots ([`g_swimlane`](https://insightsengineering.github.io/osprey/latest-tag/reference/g_swimlane.html))
- spider plots ([`g_spiderplot`](https://insightsengineering.github.io/osprey/latest-tag/reference/g_spiderplot.html))
- butterfly plots ([`g_butterfly`](https://insightsengineering.github.io/osprey/latest-tag/reference/g_butterfly.html))
- and many more
<!-- markdownlint-enable MD007 MD030 -->

## Installation

```r
# stable versions
# install.packages("pak")
pak::pkg_install("insightsengineering/osprey@*release")

# beta versions
# install.packages("pak")
pak::pkg_install("insightsengineering/osprey")
```

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/osprey.svg)](https://starchart.cc/insightsengineering/osprey)

### Stargazers

[![Stargazers repo roster for @insightsengineering/osprey](https://reporoster.com/stars/insightsengineering/osprey)](https://github.com/insightsengineering/osprey/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/osprey](https://reporoster.com/forks/insightsengineering/osprey)](https://github.com/insightsengineering/osprey/network/members)
