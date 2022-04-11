
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vaster

<!-- badges: start -->
<!-- badges: end -->

The goal of vaster is to …

## Installation

You can install the development version of vaster from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/vaster")
```

# vaster

<!-- badges: start -->
<!-- badges: end -->

The goal of vaster is to …

This is *very* barebones, while I work on related things.

There’s affinity, vapour, gdalio, and granulated - all having a say in
this. I want this basically to do

``` r
r <- raster()                                 ## or rast() for terra
r1 <- crop(r, extent(xmin, xmax, ymin, ymax)) ## ext() for terra
```

*without* all the unnecessary spatial guff. (It does what raster’s
alignExtent does, and what raster/stars/terra/spatstat/GDAL and your GIS
all do with a discretized domain when you crop it - it snaps to the
grid: in, out, or near).

See?

``` r
library(vaster)
set.seed(1)
x <- c(sort(runif(2, -20, 160)), sort(runif(2, -80, 10)))
names(x) <- c("xmin", "xmax", "ymin", "ymax")
print(x)
#>       xmin       xmax       ymin       ymax 
#>  27.791559  46.982302 -28.443197   1.738701
## all we need is a extent and dimension, we want to align to that grid
v <- vcrop(x,  c(-180, 180, -90, 90), c(360, 180) /3)
plot(NA, xlim = v$extent[1:2], ylim = v$extent[3:4], asp = "")
g_along <- function(x, n) seq(x[1], x[2], length.out = n)
abline(v = v$extent[1:2], h = v$extent[3:4], lwd = 2)
abline(v = g_along(v$extent[1:2], v$dimension[1]), h = g_along(v$extent[3:4], v$dimension[2]), col = "grey")

## these points were used to crop the existing grid, they don't define its alignment
points(x[1:2], x[3:4], pch = "+")
```

<img src="man/figures/README-vaster-1.png" width="100%" />

There is also the cells-from stuff, implemented but not checked well in
hypertidy/granulated. I’ve come just just thinking about functions with
arguments extent,dimension, because that’s all a raster is.

I might use this to create VRT templates for some cases, and possibly we
should have rasterio-alike affine support in this package.

get in touch IYI
