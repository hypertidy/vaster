# Intersect extent

Return the overlapping extent.

## Usage

``` r
intersect_extent(x, dimension, extent = NULL)
```

## Arguments

- x:

  extent to intersect

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

## Value

extent, a numeric vector of xmin,xmax,ymin,ymax

## Examples

``` r
intersect_extent(c(0.5, 2.3, 1.2, 5), c(10, 5), c(0, 10, 0, 5))
#> [1] 0 3 1 5
```
