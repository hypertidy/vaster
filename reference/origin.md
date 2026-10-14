# Origin of grid alignment

Origin of grid alignment

## Usage

``` r
origin(dimension, extent = NULL)
```

## Arguments

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

## Value

coordinate of grid origin

## Examples

``` r
origin(c(10, 5), c(0, 10, 0, 5))
#> [1] 0 0
```
