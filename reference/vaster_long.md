# Convert to long form coordinates

Matrix of xyz values in raster order.

## Usage

``` r
vaster_long(dimension, extent = NULL, data = NULL, raster_order = TRUE)
```

## Arguments

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

- data:

  data values

- raster_order:

  use raster order or native R matrix order

## Value

matrix of coordinates x,y

## Details

Use 'raster_order = FALSE' for traditional R matrix x,y order

## Examples

``` r
vaster_long(c(10, 5), c(0, 10, 0, 5))
#>         x   y
#>  [1,] 0.5 4.5
#>  [2,] 1.5 4.5
#>  [3,] 2.5 4.5
#>  [4,] 3.5 4.5
#>  [5,] 4.5 4.5
#>  [6,] 5.5 4.5
#>  [7,] 6.5 4.5
#>  [8,] 7.5 4.5
#>  [9,] 8.5 4.5
#> [10,] 9.5 4.5
#> [11,] 0.5 3.5
#> [12,] 1.5 3.5
#> [13,] 2.5 3.5
#> [14,] 3.5 3.5
#> [15,] 4.5 3.5
#> [16,] 5.5 3.5
#> [17,] 6.5 3.5
#> [18,] 7.5 3.5
#> [19,] 8.5 3.5
#> [20,] 9.5 3.5
#> [21,] 0.5 2.5
#> [22,] 1.5 2.5
#> [23,] 2.5 2.5
#> [24,] 3.5 2.5
#> [25,] 4.5 2.5
#> [26,] 5.5 2.5
#> [27,] 6.5 2.5
#> [28,] 7.5 2.5
#> [29,] 8.5 2.5
#> [30,] 9.5 2.5
#> [31,] 0.5 1.5
#> [32,] 1.5 1.5
#> [33,] 2.5 1.5
#> [34,] 3.5 1.5
#> [35,] 4.5 1.5
#> [36,] 5.5 1.5
#> [37,] 6.5 1.5
#> [38,] 7.5 1.5
#> [39,] 8.5 1.5
#> [40,] 9.5 1.5
#> [41,] 0.5 0.5
#> [42,] 1.5 0.5
#> [43,] 2.5 0.5
#> [44,] 3.5 0.5
#> [45,] 4.5 0.5
#> [46,] 5.5 0.5
#> [47,] 6.5 0.5
#> [48,] 7.5 0.5
#> [49,] 8.5 0.5
#> [50,] 9.5 0.5
# see https://gist.github.com/mdsumner/b844766f28910a3f87dc2c8a398a3a13
```
