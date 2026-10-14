# Partition a grid into tiles

Pure index arithmetic: how many tiles fit and what their extents are.
Tiles are enumerated left to right, then top to bottom, matching cell
order. Edge tiles may be smaller than `tile_dim`. Every tile is aligned
to the parent and
[`grid_offset()`](https://hypertidy.github.io/vaster/reference/grid_offset.md)
of a tile is its origin in parent cells.

## Usage

``` r
grid_tiles(dimension, extent = NULL, tile_dim = c(256L, 256L))
```

## Arguments

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

- tile_dim:

  tile dimension (ncol, nrow), recycled if length 1

## Value

list of lists, each with `dimension` and `extent`, plus `offset` (the
tile's 0-based `c(col, row)` origin in the parent)

## Examples

``` r
tl <- grid_tiles(c(10L, 7L), c(0, 10, 0, 7), c(4L, 4L))
length(tl)
#> [1] 6
tl[[6]]
#> $dimension
#> [1] 2 3
#> 
#> $extent
#> [1]  8 10  0  3
#> 
#> $offset
#> col row 
#>   8   4 
#> 
```
