# Grid boundary cell index

This is for indexing coordinate arrays to get their values (the cell
index of the outer row and columns).

## Usage

``` r
vaster_boundary_cell(dimension)
```

## Arguments

- dimension:

  integer ncol, nrow

## Value

a matrix of xy coordinates

## Details

The orientation is the same as for
[`vaster_boundary()`](https://hypertidy.github.io/vaster/reference/vaster_boundary.md).

## Examples

``` r
vaster_boundary_cell(c(3, 4))
#>  [1] 10 11 12 12  9  6  3  3  2  1  1  4  7 10
cell <- vaster_boundary_cell(c(3, 4))
plot(vaster_boundary(c(3, 4)))
text(xy <- xy_from_cell(c(3, 4), cell = vaster_boundary_cell(c(3, 4))), lab = cell)
lines(xy)
```
