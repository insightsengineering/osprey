

## Osprey

Package for entry level R-development of TLG:s. Webpage of all the TLG:s in the package: https://pages.github.roche.com/Rpackages/osprey/

###Requirements:

To quolify to Osprey package, TLG code has to
1. Be a function that produces a output
2. Have working example 
3. Use Oxygen for creation of Help
4. Have help page 
5. Be compatible with pkgdown (should be when points 1-4 are OK)

###Instructions
####for this to work you need to have access to libraries pkgdown and random.cdisc.data
1. Create branch of Osprey and clone to R-studio
2. Develop a function
3. Create oxygen comments (ctrl+shift+alt+R or copy from earlier)
4. Document (ctrl+shift+D)
5. Clean and Rebuild
6. Test your function with working example
7. Create webpage (in console: pkgdown::build_site())
8. When everything works, make pull request.

###Example plot

https://github.roche.com/Rpackages/osprey/blob/master/R/dummyplot.R

###Install with

```r
devtools::install_github(...)
```
