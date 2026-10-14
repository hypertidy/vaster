# Aspect ratio of dimension conflated with bbox

Generate an appropriate dimension (shape, ncol,nrow) from an input
width(height). If height not specified we have a square.

## Usage

``` r
fit_dims(size = 1024L, wh = c(size, size))
```

## Arguments

- size:

  seed dimension size

- wh:

  distance across dimension span/s

## Value

dimension `c(ncol, nrow)`

## Examples

``` r
fit_dims(256, c(10, 20))
#> [1] 128 256
fit_dims(1024, c(102723, 1e5))
#> [1] 1024  997
```
