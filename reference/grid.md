# Grid

Basic grid tools, cell, resolution, dimension, extent.

## Usage

``` r
n_cell(dimension)

x_res(dimension, extent = NULL)

y_res(dimension, extent = NULL)

n_row(dimension)

n_col(dimension)

xlim(dimension, extent = NULL)

ylim(dimension, extent = NULL)

x_min(dimension, extent = NULL)

x_max(dimension, extent = NULL)

y_min(dimension, extent = NULL)

y_max(dimension, extent = NULL)
```

## Arguments

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

## Value

number of cells

x resolution (width of cell)

y resolution (height of cell)

number of rows

number of cols

x extent (corner to corner)

y extent (corner to corner)

x minimum (left edge)

x maximum (right edge)

y minimum (bottom edge)

y maximum (top edge)

## Examples

``` r
n_cell(c(10, 5))
#> [1] 50
x_res(c(10, 5), c(0, 10, 0, 5))
#> [1] 1
y_res(c(10, 5), c(0, 10, 0, 5))
#> [1] 1
n_row(c(10, 5))
#> [1] 5
n_col(c(10, 5))
#> [1] 10
xlim(c(10, 5), c(0, 10, 0, 5))
#> [1]  0 10
ylim(c(10, 5), c(0, 10, 0, 5))
#> [1] 0 5
x_min(c(10, 5), c(0, 10, 0, 5))
#> [1] 0
x_max(c(10, 5), c(0, 10, 0, 5))
#> [1] 10
y_min(c(10, 5), c(0, 10, 0, 5))
#> [1] 0
y_max(c(10, 5), c(0, 10, 0, 5))
#> [1] 5
```
