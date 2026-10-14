# Integer offset between two grids of equal resolution

The column and row offset from the top-left cell of the parent grid to
the top-left cell of the child grid. Both grids must have the same
resolution and share a lattice. The offset may be negative or exceed the
parent dimension: alignment is not containment.

## Usage

``` r
grid_offset(
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

named integer vector `c(col = , row = )`, 0-based

## Examples

``` r
## child starts 2 cells in and 3 cells down
grid_offset(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(2, 7, 2, 7))
#> col row 
#>   2   3 
```
