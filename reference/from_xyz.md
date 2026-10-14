# Derive a grid from XYZ points

This function is very liberal, it simply finds unique x values and
unique y values, sorts them and finds the minimum difference between the
points, then checks that rounded ratio of differences to this minimum is
1.

## Usage

``` r
from_xyz(xyz, digits = 5)
```

## Arguments

- xyz:

  set of points xy or xyz (matrix or data frame)

- digits:

  argument passed to [`round()`](https://rdrr.io/r/base/Round.html)

## Value

list with elements 'dimension', 'extent'

## Details

The points can be the full grid set, a partial set, or a superset of the
grid. The resolution will simply be the smallest actual difference
found. (Zero is not possible because we `sort(unique(...))`).

The z-column if present is ignored.

## Examples

``` r
from_xyz(vaster_long(c(10, 5), c(0, 10, 0, 5)))
#> $dimension
#> [1] 10  5
#> 
#> $extent
#> [1]  0 10  0  5
#> 
```
