# Cells

Functions that work with cells.

## Usage

``` r
cell_from_xy(dimension, extent = NULL, xy)

cell_from_extent(dimension, extent = NULL, x_extent)

extent_from_cell(dimension, extent = NULL, cell)

rowcol_from_cell(dimension, extent = NULL, cell)

xy_from_cell(dimension, extent = NULL, cell)

x_from_cell(dimension, extent = NULL, cell)

y_from_cell(dimension, extent = NULL, cell)

col_from_cell(dimension, cell)

row_from_cell(dimension, cell)

cell_from_row(dimension, row)

cell_from_col(dimension, col)

cell_from_row_col(dimension, row, col)

cell_from_rowcol_combine(dimension, row, col)
```

## Arguments

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

- xy:

  matrix of coordinates

- x_extent:

  extent to find cells of

- cell:

  cells to find extent, or row,col, or xy of

- row:

  row to find cell of

- col:

  column to find cell of

## Value

cell index

cells of extent

extent of cells

row,col of cells

xy from cells

x of cells

y of cells

col of cells

row of cells

cell of rows

cell of cols

cell of row,col

cell of row,col combined

## Details

The cell is indexed from the top left corner and proceeds to the right,
and then down scanning by rows. The n cell is a the bottom right corner.
Orientation is different to R's native matrix order, but see (WiP doc
and helpers for conversion).

## Examples

``` r
cell_from_xy(c(10, 5), extent = c(0, 10, 0, 5), cbind(5, 4))
#> [1] 16
cell_from_extent(c(10, 5), c(0, 10, 0, 5), c(6, 7, 2, 3))
#> [1] 27
extent_from_cell(c(10, 5), c(0, 10, 0, 5), c(4, 5))
#> [1] 3 5 4 5
rowcol_from_cell(c(10, 5), c(0, 10, 0, 5), 3:5)
#>      row col
#> [1,]   1   3
#> [2,]   1   4
#> [3,]   1   5
xy_from_cell(c(10, 5), c(0, 10, 0, 5), 4:6)
#>      [,1] [,2]
#> [1,]  3.5  4.5
#> [2,]  4.5  4.5
#> [3,]  5.5  4.5
x_from_cell(c(10, 5), c(0, 10, 0, 5), 4:7)
#> [1] 3.5 4.5 5.5 6.5
y_from_cell(c(10, 5), c(0, 10, 0, 5), 4:7)
#> [1] 4.5 4.5 4.5 4.5
col_from_cell(c(10, 5),  4:7)
#> [1] 4 5 6 7
row_from_cell(c(10, 5),  4:7)
#> [1] 1 1 1 1
cell_from_row(c(10, 5), 4:7)
#>  [1] 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 NA NA NA NA NA
#> [26] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
cell_from_col(c(10, 5), 4:7)
#>  [1]  4 14 24 34 44  5 15 25 35 45  6 16 26 36 46  7 17 27 37 47
cell_from_row_col(c(10, 5), 1:4, 4:7)
#> [1]  4 15 26 37
cell_from_rowcol_combine(c(10, 5), 1:4, 4:7)
#>  [1]  4  5  6  7 14 15 16 17 24 25 26 27 34 35 36 37
```
