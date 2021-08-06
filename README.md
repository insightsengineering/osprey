

## osprey

`osprey` is an R package for crowd sourcing development of analysis functions to
create TLGs. We also provide teal modules for outputs in `osprey` in the
`teal.osprey` package.

## Installation

Please install package dependencies as follows:

### Stable Version

```r
devtools::install_github(
  repo = "insightsengineering/scda",
  ref = "releases",
  host = "https://github.com/api/v3",
  upgrade_dependencies = FALSE
)
devtools::install_github(
  repo = "insightsengineering/scda.2021",
  ref = "releases",
  host = "https://github.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github("Roche/rtables")

devtools::install_github(
  repo = "insightsengineering/tern",
  ref = "releases",
  host = "https://github.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "insightsengineering/osprey",
  ref = "releases",
  host = "https://github.com/api/v3",
  upgrade_dependencies = FALSE
)
```

### Development Version

```r
devtools::install_github(
  repo = "insightsengineering/scda",
  ref = "main",
  host = "https://github.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "insightsengineering/scda.2021",
  ref = "main",
  host = "https://github.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "Roche/rtables",
  ref = "main",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "insightsengineering/tern",
  ref = "main",
  host = "https://github.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "insightsengineering/osprey",
  ref = "main",
  host = "https://github.com/api/v3",
  upgrade_dependencies = FALSE
)
```


## Development

Everyone is welcome to share their R functions in `osprey`.

### Requirements

To qualify to `osprey` package, TLG code has to:

1. Be a function that produces an output
2. Have a working example
3. Use [roxygen documentation](http://r-pkgs.had.co.nz/man.html) for creation of Help
4. Have a help page
5. Be compatible with pkgdown (should be when points 1-4 are OK)

### Instructions

For this to work you need to have access to libraries `pkgdown` and
`scda`.

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

# Contributors:

- **Nina Qi (qit3@gene.com)**
- Chendi Liao (chendi.liao@roche.com)
- Mahdi About (mahdi.about@roche.com)
- Fei Wang (fei.wang.fw6@roche.com)
- Mika Maekinen (mika.maekinen@roche.com)
- Carolyn Zhang (zhang.carolyn@gene.com)
