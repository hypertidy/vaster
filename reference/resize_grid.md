# Resize a grid by a factor, keeping the extent

Change resolution by a factor that yields whole-number dimensions. The
new lattice nests exactly within the old one (factor \> 1) or the old
within the new (factor \< 1), so `resize_grid(resize_grid(g, k), 1/k)`
returns `g`.

## Usage

``` r
resize_grid(dimension, extent = NULL, factor, tol = lattice_tol())
```

## Arguments

- dimension:

  integer ncol, nrow

- extent:

  numeric extent xmin,xmax,ymin,ymax

- factor:

  positive scaling factor, applied to the dimension; a single value or
  `c(xfactor, yfactor)`

- tol:

  relative tolerance for whole-number tests

## Value

list with `dimension` and `extent`

## Examples

``` r
resize_grid(c(10L, 10L), c(0, 10, 0, 10), 2)
#> $dimension
#> [1] 20 20
#> 
#> $extent
#> [1]  0 10  0 10
#> 
resize_grid(c(10L, 10L), c(0, 10, 0, 10), 0.5)
#> $dimension
#> [1] 5 5
#> 
#> $extent
#> [1]  0 10  0 10
#> 
try(resize_grid(c(10L, 10L), c(0, 10, 0, 10), 0.3))
#> $dimension
#> [1] 3 3
#> 
#> $extent
#> [1]  0 10  0 10
#> 
```
