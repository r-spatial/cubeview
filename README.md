
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cubeview - Interactively Explore 3D Raster Data Cubes

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-spatial/cubeview/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-spatial/cubeview/actions/workflows/R-CMD-check.yaml)
[![cran
checks](https://badges.cranchecks.info/worst/cubeview.svg)](https://cran.r-project.org/web/checks/check_results_cubeview.html)
![monthly](https://cranlogs.r-pkg.org/badges/cubeview)
![total](https://cranlogs.r-pkg.org/badges/grand-total/cubeview)
[![CRAN](https://www.r-pkg.org/badges/version/cubeview?color=009999)](https://cran.r-project.org/package=cubeview)
<!-- badges: end -->

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
Code of
Conduct](https://github.com/r-spatial/cubeview/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
