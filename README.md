
# osprey

`osprey` is an R package for community contributed analysis functions to
create TLGs (tables, listing and graphs) for analysis of clinical trial data.
These functions be used stand alone but are also called by the
[teal.osprey](https://github.com/insightsengineering/teal.osprey) package which provides `teal` modules to be used
inside `teal` applications.

The package provides
- waterfall plots (`g_waterfall`)
- swim lane plots (`g_swimlane`)
- spider plots (`g_spiderplot`)
- butterfly plots (`g_butterfly`)
- and many more

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("osprey")
devtools::install_github("insightsengineering/osprey@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

In order to run many of the examples you will also need to install the [scda](https://github.com/insightsengineering/scda) package.

## Contributing to `osprey`

Everyone is welcome to share their R functions in `osprey`.

### Requirements

To qualify to `osprey` package, TLG code has to:

1. Be a function that produces an output
2. Have a working example
3. Use [roxygen documentation](http://r-pkgs.had.co.nz/man.html) for creation of Help
4. Have a help page
5. Be compatible with pkgdown (should be when points 1-4 are OK)

### Instructions

For this to work you need to have access to libraries `pkgdown` and `scda`.

That is, follow the following steps:

1. Clone the osprey repository
2. Create a branch
3. Develop a function
4. Create roxygen comments (ctrl+shift+alt+R or copy from earlier)
5. Document (ctrl+shift+D)
6. Install and Restart
7. Test your function with working example
8. Create webpage (in console: `pkgdown::build_site()`)
9. When everything works, make pull request.
