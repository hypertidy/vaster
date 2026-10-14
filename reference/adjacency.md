# Cell adjacency

Find neighbouring cells by index, using only the grid dimension.

## Usage

``` r
adjacency(dimension, cell, directions = "queen")
```

## Arguments

- dimension:

  integer vector of ncol, nrow

- cell:

  integer vector of cell indices (1-based)

- directions:

  character, one of `"queen"`, `"rook"`, or `"bishop"`

## Value

integer matrix with one row per cell and one column per neighbour
direction. Column names indicate the direction. Out-of-bounds neighbours
are `NA`.

## Details

Given cell indices into a grid of known dimension, return the indices of
neighbouring cells. Out-of-bounds neighbours (at grid edges) are `NA`.

The `directions` argument controls which neighbours are returned:

- `"queen"` (default): all 8 neighbours (rook + bishop)

- `"rook"`: 4 cardinal neighbours (up, down, left, right)

- `"bishop"`: 4 diagonal neighbours

Column order for `"queen"` is: up, down, left, right, upleft, upright,
downleft, downright. For `"rook"`: up, down, left, right. For
`"bishop"`: upleft, upright, downleft, downright. Direction names refer
to the raster convention where cell 1 is at the top-left.

## Corner vertex values from area-based rasters

A key use case is converting area-based cell values to corner vertex
values for mesh generation. Each corner vertex is shared by up to four
cells. For example, the top-left corner of a cell is shared with the
cell above, the cell to the left, and the cell diagonally above-left:

      upleft  |  up
      --------x------     vertex 'x' = mean of upleft, up, left, self
      left    | self

The queen-connected neighbours give all the cells needed to compute
every corner vertex. Averaging the appropriate neighbours provides
weighted corner values from flat pixel areas, which is the basis for
constructing continuous meshes from raster data (as used by quadmesh and
anglr).

## Examples

``` r
## 4x3 grid (4 columns, 3 rows), 12 cells
adjacency(c(4, 3), cell = 6)
#>      up down left right upleft upright downleft downright
#> [1,]  2   10    5     7      1       3        9        11
adjacency(c(4, 3), cell = 6, directions = "rook")
#>      up down left right
#> [1,]  2   10    5     7
adjacency(c(4, 3), cell = 6, directions = "bishop")
#>      upleft upright downleft downright
#> [1,]      1       3        9        11

## corner cell has fewer valid neighbours
adjacency(c(4, 3), cell = 1)
#>      up down left right upleft upright downleft downright
#> [1,] NA    5   NA     2     NA      NA       NA         6

## multiple cells at once
adjacency(c(4, 3), cell = 1:12)
#>       up down left right upleft upright downleft downright
#>  [1,] NA    5   NA     2     NA      NA       NA         6
#>  [2,] NA    6    1     3     NA      NA        5         7
#>  [3,] NA    7    2     4     NA      NA        6         8
#>  [4,] NA    8    3    NA     NA      NA        7        NA
#>  [5,]  1    9   NA     6     NA       2       NA        10
#>  [6,]  2   10    5     7      1       3        9        11
#>  [7,]  3   11    6     8      2       4       10        12
#>  [8,]  4   12    7    NA      3      NA       11        NA
#>  [9,]  5   NA   NA    10     NA       6       NA        NA
#> [10,]  6   NA    9    11      5       7       NA        NA
#> [11,]  7   NA   10    12      6       8       NA        NA
#> [12,]  8   NA   11    NA      7      NA       NA        NA

## ------------------------------------------------
## Corner vertex interpolation from area values
## ------------------------------------------------
dm <- c(4, 3)
elev <- c(10, 12, 14, 16, 11, 13, 15, 17, 10, 11, 13, 14)

## The top-left corner of each cell is shared by self, the
## cell above, the cell to the left, and the cell diagonally
## above-left. Average these for the vertex value:
nb <- adjacency(dm, seq_along(elev))
vals <- cbind(elev, elev[nb[, "up"]],
              elev[nb[, "left"]], elev[nb[, "upleft"]])
corner <- rowMeans(vals, na.rm = TRUE)
matrix(corner, nrow = 3, byrow = TRUE)
#>      [,1]  [,2] [,3]  [,4]
#> [1,] 10.0 11.00 13.0 15.00
#> [2,] 10.5 11.50 13.5 15.50
#> [3,] 10.5 11.25 13.0 14.75
```
