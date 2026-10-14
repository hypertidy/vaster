# Geo transform parameter creator

Basic function to create a geotransform as used by GDAL.

## Usage

``` r
geo_transform0(px, ul, sh = c(0, 0))
```

## Arguments

- px:

  pixel resolution (XY, Y-negative)

- ul:

  grid offset, top-left corner

- sh:

  affine shear (XY)

## Value

vector of parameters xmin, xres, yskew, ymax, xskew, yres

## See also

[`geo_world0()`](https://hypertidy.github.io/vaster/reference/geo_world0.md)
which uses the same parameters in a different order

## Examples

``` r
geo_transform0(px = c(1, -1), ul = c(0, 0))
#>  xmin  xres yskew  ymax xskew  yres 
#>     0     1     0     0     0    -1 
```
