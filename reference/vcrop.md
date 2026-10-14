# Virtual grid modification

To modify a grid is to align an extent to the grid origin. Modification
includes reducing or extending the area covered in either dimension.
This implies a new extent, snapped to the grain of the origin grid, and
a new dimension (ncol, nrow).

## Usage

``` r
vcrop(x, dimension, extent = NULL, ..., snap = "out")
```

## Arguments

- x:

  extent of candidate grid (vector of xmin, xmax, ymin, ymax)

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

- ...:

  ignored

- snap:

  one of "out" (default), "near", or "in"

## Value

A list with two components:

- extent:

  numeric vector (xmin, xmax, ymin, ymax) - the new extent, snapped to
  grid alignment

- dimension:

  integer vector (ncol, nrow) - the dimension of the modified grid

## Details

This works for any grid: the input extent can be within the original, an
extension of the original, or completely non-intersecting the original
grid.

## See also

[`align_extent()`](https://hypertidy.github.io/vaster/reference/align_extent.md)
for just the extent snapping,
[`extent_dimension()`](https://hypertidy.github.io/vaster/reference/extent_dimension.md)
for just the dimension calculation

## Examples

``` r
## any arbitrary extent
(x <- c(sort(runif(2, -180, 180)), sort(runif(2, -90, 90))))
#> [1]  83.83752  98.10774 -58.51069  67.42812

vcrop(x, c(360, 180), c(-180, 180, -90, 90))
#> $extent
#> [1]  83  99 -59  68
#> 
#> $dimension
#> [1]  16 127
#> 

## crop to a smaller region
vcrop(c(0, 10, 0, 10), c(360, 180), c(-180, 180, -90, 90))
#> $extent
#> [1]  0 10  0 10
#> 
#> $dimension
#> [1] 10 10
#> 

## extend beyond original (snapped to grid)
vcrop(c(-200, 200, -100, 100), c(360, 180), c(-180, 180, -90, 90))
#> $extent
#> [1] -200  200 -100  100
#> 
#> $dimension
#> [1] 400 200
#> 
```
