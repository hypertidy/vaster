# Snap a grid onto a parent lattice

Moves the extent of the first grid so that its cell boundaries fall on
cell boundaries of the parent grid, keeping its own resolution. The two
resolutions must be commensurate. The snap is to the coarser of the two
resolutions, anchored at the parent's top-left origin, so the result is
always aligned to the parent (and the parent to it).

## Usage

``` r
snap_grid(
  dimension,
  extent = NULL,
  dimension2,
  extent2 = NULL,
  tol = lattice_tol()
)
```

## Arguments

- dimension:

  dimension of the grid to snap (ncol, nrow)

- extent:

  extent of the grid to snap

- dimension2:

  dimension of the parent grid

- extent2:

  extent of the parent grid

- tol:

  relative tolerance for whole-number tests

## Value

list with `dimension` (unchanged) and `extent` (snapped)

## Details

This differs from
[`align_extent()`](https://hypertidy.github.io/vaster/reference/align_extent.md)
/ [`vcrop()`](https://hypertidy.github.io/vaster/reference/vcrop.md),
which snap an arbitrary extent to a grid and take that grid's
resolution. Here the grid being snapped keeps its resolution and only
its origin moves.

## Examples

``` r
## 2x2 grid at res 2, slightly off a unit lattice
snap_grid(c(2L, 2L), c(0.4, 4.4, 0.4, 4.4), c(10L, 10L), c(0, 10, 0, 10))
#> $dimension
#> [1] 2 2
#> 
#> $extent
#> [1] 0 4 0 4
#> 
```
