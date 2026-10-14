# World file parameter creator

Basic function to create a ['world
file'](https://en.wikipedia.org/wiki/World_file) as used by various
non-geo image formats

Reformat to world vector.

## Usage

``` r
geo_world0(px, ul, sh = c(0, 0))

geotransform_to_world(x)
```

## Arguments

- px:

  pixel resolution (XY, Y-negative)

- ul:

  grid offset, top-left corner

- sh:

  affine shear (XY)

- x:

  geotransform parameters, as per
  [`geo_transform0()`](https://hypertidy.github.io/vaster/reference/geo_transform0.md)

## Value

vector of parameters xres, yskew, xskew, yres, xmin, ymax

world vector, as per `geo_world0()`

## Details

Note that xmin/xmax are *centre_of_cell* (of top-left cell) unlike the
geotransform which is top-left *corner_of_cell*. The parameters are
otherwise the same, but in a different order.

## See also

[geo_transform0](https://hypertidy.github.io/vaster/reference/geo_transform0.md)

## Examples

``` r
geo_world0(px = c(1, -1), ul = c(0, 0))
#>  xres yskew xskew  yres  xmin  ymax 
#>   1.0   0.0   0.0  -1.0   0.5  -0.5 
(gt <- geo_transform0(px = c(1, -1), ul = c(0, 0)))
#>  xmin  xres yskew  ymax xskew  yres 
#>     0     1     0     0     0    -1 
wf <- geotransform_to_world(gt)
world_to_geotransform(wf)
#>  xmin  xres yskew  ymax xskew  yres 
#>     0     1     0     0     0    -1 
```
