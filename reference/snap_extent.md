# Snap extent to resolution (buffer extent)

Whole grain buffers.

## Usage

``` r
snap_extent(x, res)

buffer_extent(x, res)
```

## Arguments

- x:

  extent (xmin, xmax, ymin, ymax)

- res:

  resolution (a grain to align to)

## Value

extent, snapped to the resolution

## Examples

``` r
snap_extent(sort(rnorm(4)), 0.01)
#> [1] -2.44 -1.40 -0.01  0.26
```
