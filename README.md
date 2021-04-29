

## osprey

`osprey` is a R package for crowd sourcing development of analysis functions to
create TLGs. We also provide teal modules for outputs in `osprey` in the
teal.osprey.

There are two versions of webpage manual for this package, both the development version and the stable release version 
can be found [here](http://go.roche.com/nest-docs/api-reference/). Use the drop down menu on the top right to choose the
desired version.

## Installation

Please install package dependencies as follows:

### Stable Version

To access the stable version manual, select the latest version in the drop down menu on the top right of the [Web Manual](http://go.roche.com/nest-docs/api-reference/).

```r
devtools::install_github(
  repo = "NEST/random.cdisc.data",
  ref = "master",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github("Roche/rtables")

devtools::install_github(
  repo = "NEST/tern",
  ref = "master",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "NEST/osprey",
  ref = "master",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```

### Development Version

To access the development version manual, select `devel` in the drop down menu on the top right of the [Web Manual](http://go.roche.com/nest-docs/api-reference/).

```r
devtools::install_github(
  repo = "NEST/random.cdisc.data",
  ref = "master",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "Roche/rtables",
  ref = "devel",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "NEST/tern",
  ref = "devel",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github(
  repo = "NEST/osprey",
  ref = "devel",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```


## Development

Everyone is welcome to share their R functions in `osprey`.

### Requirements

To qualify to `osprey` package, TLG code has to:

1. Be a function that produces a output
2. Have working example
3. Use [roxygen documentation](http://r-pkgs.had.co.nz/man.html) for creation of Help
4. Have help page
5. Be compatible with pkgdown (should be when points 1-4 are OK)

### Instructions

For this to work you need to have access to libraries `pkgdown` and
`random.cdisc.data`. Please start by watching the following video:

* [Add a TLG function to the osprey R
package](https://streamingmedia.roche.com/media/Adding+TLG+functions+to+the+Osprey+R+package/1_4newkk7i)

That is, follow the following steps:

1. Clone the osprey repository
2. Create branch
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
