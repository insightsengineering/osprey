

## Osprey

Package for entry level R-development of TLG:s. Webpage of all the TLG:s in the package: https://pages.github.roche.com/Rpackages/osprey/


## Installation

### Stable Release

[Web Manual](https://pages.github.roche.com/Rpackages/osprey/)

```r
devtools::install_github(repo = "Rpackages/osprey", ref = "release-tag", host = "https://github.roche.com/api/v3")
```

### Development Version

[Web Manual](https://pages.github.roche.com/Rpackages/osprey/dev/)

```r
devtools::install_github(repo = "Rpackages/osprey", host = "https://github.roche.com/api/v3")
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

For this to work you need to have access to libraries `pkgdown` and `random.cdisc.data`. Please start by watching the following video:

* [Add a TLG function to the Osrpey R package](https://streamingmedia.roche.com/media/Adding+TLG+functions+to+the+Osprey+R+package/1_4newkk7i)

That is, follow the following steps:

1. Clone the opsrey repository 
1. Create branch
1. Develop a function
1. Create roxygen comments (ctrl+shift+alt+R or copy from earlier)
1. Document (ctrl+shift+D)
1. Install and Restart
1. Test your function with working example
1. Create webpage (in console: `pkgdown::build_site()`)
1. When everything works, make pull request.

### Example plot

https://github.roche.com/Rpackages/osprey/blob/master/R/dummyplot.R


