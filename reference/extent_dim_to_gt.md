# Create geotransform from extent and dimension

Create the geotransform (see
[`geo_transform0()`](https://hypertidy.github.io/vaster/reference/geo_transform0.md))
from extent and dimension.

## Usage

``` r
extent_dim_to_gt(x, dimension)
```

## Arguments

- x:

  extent parameters, c(xmin,xmax,ymin,ymax)

- dimension:

  dimensions x,y of grid (ncol,nrow)

## Value

6-element
[`geo_transform0()`](https://hypertidy.github.io/vaster/reference/geo_transform0.md)

## Details

The dimension is always ncol, nrow.

## Examples

``` r
extent_dim_to_gt(c(0, 5, 0, 10), c(5, 10))
#>  xmin  xres yskew  ymax xskew  yres 
#>     0     1     0    10     0    -1 
```
