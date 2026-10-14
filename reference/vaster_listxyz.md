# Image xyz list

Generate list of x and y rectilinear coordinates with z matrix.

## Usage

``` r
vaster_listxyz(dimension, extent = NULL, data = NULL)
```

## Arguments

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

- data:

  data values (length of the product of 'dimension')

## Value

list with elements x,y,z as per
[graphics::image](https://rdrr.io/r/graphics/image.html)

## Details

The rectilinear coordinates are degenerate (just a product of
extent/dimension).

## Examples

``` r
vaster_listxyz(c(10, 5), c(0, 10, 0, 5))
#> $x
#>  [1] 0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5
#> 
#> $y
#> [1] 0.5 1.5 2.5 3.5 4.5
#> 
#> $z
#>        [,1]  [,2]  [,3]  [,4]  [,5]
#>  [1,] FALSE FALSE FALSE FALSE FALSE
#>  [2,] FALSE FALSE FALSE FALSE FALSE
#>  [3,] FALSE FALSE FALSE FALSE FALSE
#>  [4,] FALSE FALSE FALSE FALSE FALSE
#>  [5,] FALSE FALSE FALSE FALSE FALSE
#>  [6,] FALSE FALSE FALSE FALSE FALSE
#>  [7,] FALSE FALSE FALSE FALSE FALSE
#>  [8,] FALSE FALSE FALSE FALSE FALSE
#>  [9,] FALSE FALSE FALSE FALSE FALSE
#> [10,] FALSE FALSE FALSE FALSE FALSE
#> 
## see https://gist.github.com/mdsumner/b844766f28910a3f87dc2c8a398a3a13
```
