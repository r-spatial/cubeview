
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cubeview - Interactively Explore 3D Raster Data Cubes

[![Travis build
status](https://travis-ci.org/r-spatial/cubeview.svg?branch=master)](https://travis-ci.org/r-spatial/cubeview)
[![monthly](http://cranlogs.r-pkg.org/badges/cubeview)](https://www.rpackages.io/package/cubeview)
[![total](http://cranlogs.r-pkg.org/badges/grand-total/cubeview)](https://www.rpackages.io/package/cubeview)
[![CRAN](http://www.r-pkg.org/badges/version/cubeview?color=009999)](https://cran.r-project.org/package=cubeview)
[![status](https://tinyverse.netlify.com/badge/cubeview)](https://CRAN.R-project.org/package=cubeview)

`cubeview` enables interactive 3D exploration of raster data cubes.

## Installation

You can install the released version of cubeview from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cubeview")
```

## Example

``` r
library(raster)

kili_data <- system.file("extdata", "kiliNDVI.tif", package = "cubeview")
kiliNDVI <- stack(kili_data)

cubeview(kiliNDVI)
```

![](man/figures/README-kili_cube.png)

### Code of Conduct

Please note that the ‘cubeview’ project is released with a [Contributor
Code of Conduct](https://github.com/r-spatial/cubeview/blob/master/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.
