

## Osprey

`osprey` is a R package for crowd sourcing development of analysis functions to
create TLGs. We also provide [teal](https://github.roche.com/Rpackages/teal)
modules for outputs in `osprey` in the
[teal.osprey](https://github.roche.com/Rpackages/teal.osprey) R package.

There are two versions of webpage manual for this package

* development version: https://pages.github.roche.com/Rpackages/osprey/dev/

* stable release version: https://pages.github.roche.com/Rpackages/osprey/


## Installation

Please install package dependencies as follows:

### Stable Release

[Web Manual](https://pages.github.roche.com/Rpackages/osprey/)

```r 
devtools::install_github(repo = "Roche/rtables", ref = "v0.1.0.5",
  upgrade_dependencies = FALSE)

devtools::install_github(repo = "Rpackages/tern", ref = "v0.5.0.3", 
  host = "https://github.roche.com/api/v3", upgrade_dependencies = FALSE)

devtools::install_github(repo = "Rpackages/osprey", ref = "v0.1.0", 
  host = "https://github.roche.com/api/v3", upgrade_dependencies = FALSE)
```

### Development Version

[Web Manual](https://pages.github.roche.com/Rpackages/osprey/dev/)

```r
devtools::install_github("Roche/rtables", ref = "master",
  upgrade_dependencies = FALSE)

devtools::install_github( repo = "Rpackages/tern", ref = "master", 
  host = "https://github.roche.com/api/v3", upgrade_dependencies = FALSE )

devtools::install_github(repo = "Rpackages/osprey", ref = "master",
  host = "https://github.roche.com/api/v3", upgrade_dependencies = FALSE) 
``` 


## Development

Everyone is welcome to share their R functions in `opsrey`.

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

* [Add a TLG function to the Osrpey R
package](https://streamingmedia.roche.com/media/Adding+TLG+functions+to+the+Osprey+R+package/1_4newkk7i)

That is, follow the following steps:

1. Clone the opsrey repository 
2. Create branch 
3. Develop a function 
4. Create roxygen comments (ctrl+shift+alt+R or copy from earlier) 
5. Document (ctrl+shift+D) 
6. Install and Restart 
7. Test your function with working example
8. Create webpage (in console: `pkgdown::build_site()`) 
9. When everything works, make pull request.
