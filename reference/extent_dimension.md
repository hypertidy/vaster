# Dimension for an aligned extent

input is the output of align_extent

## Usage

``` r
extent_dimension(x, dimension, extent = NULL, snap = "out")
```

## Arguments

- x:

  and aligned extent

- dimension:

  dimension of parent

- extent:

  of parent

- snap:

  out by default, may be near or in

## Value

dimension

## Examples

``` r
extent_dimension(c(.2, .8, 1.8, 3.2), c(10, 5), c(0, 10, 0, 5))
#> [1] 1 3
```
