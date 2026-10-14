# Lattice alignment of two grids

Two grids share a lattice when their cell boundaries coincide: the
resolutions are commensurate (one is an exact integer multiple of the
other, in x and in y) and the origins differ by a whole number of the
finer cells. When this holds every index operation between the grids is
exact integer arithmetic. The test is symmetric.

## Usage

``` r
is_aligned(
  dimension,
  extent = NULL,
  dimension2,
  extent2 = NULL,
  tol = lattice_tol()
)
```

## Arguments

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

- dimension2:

  dimension of the second grid (ncol, nrow)

- extent2:

  extent of the second grid (xmin, xmax, ymin, ymax), `NULL` for unit
  cell space

- tol:

  relative tolerance for whole-number tests

## Value

logical

## Details

Note that alignment is not containment: a grid may be aligned to another
and lie entirely outside it. Use
[`intersect_extent()`](https://hypertidy.github.io/vaster/reference/intersect_extent.md)
for overlap.

## See also

[`grid_offset()`](https://hypertidy.github.io/vaster/reference/grid_offset.md),
[`snap_grid()`](https://hypertidy.github.io/vaster/reference/snap_grid.md),
[`resize_grid()`](https://hypertidy.github.io/vaster/reference/resize_grid.md),
[`grid_tiles()`](https://hypertidy.github.io/vaster/reference/grid_tiles.md)

## Examples

``` r
is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(2, 7, 2, 7))
#> [1] TRUE
is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(2.5, 7.5, 2, 7))
#> [1] FALSE
## coarser grid on the same lattice
is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(2L, 2L), c(0, 10, 0, 10))
#> [1] TRUE
```
