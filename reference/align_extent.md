# Crop an extent, snapped to the grain

A crop (or extend), it snaps the input extent to the origin of the input
extent (based on the dimension) \#' Note that snap is modelled on the
behaviour of the raster package, and is different from projwin in GDAL
(WIP to illustrate).

## Usage

``` r
align_extent(x, dimension, extent = NULL, snap = c("out", "near", "in"))
```

## Arguments

- x:

  extent

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

- snap:

  out by default, may be near or in

## Value

aligned extent

## Examples

``` r
align_extent(c(4.5, 5.6, 2, 4), c(10, 5), c(0, 10, 0, 5))
#> [1] 4 6 2 4
```
