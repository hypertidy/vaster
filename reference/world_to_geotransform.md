# Create geotransform from world vector

Convert world vector (centre offset) and x,y spacing to geotransform
format.

## Usage

``` r
world_to_geotransform(x)
```

## Arguments

- x:

  worldfile parameters, as per
  [`geo_world0()`](https://hypertidy.github.io/vaster/reference/geo_world0.md)

## Value

geotransform vector, see
[`geo_transform0()`](https://hypertidy.github.io/vaster/reference/geo_transform0.md)

## Examples

``` r
(wf <- geo_world0(px = c(1, -1), ul = c(0, 0)))
#>  xres yskew xskew  yres  xmin  ymax 
#>   1.0   0.0   0.0  -1.0   0.5  -0.5 
gt <- world_to_geotransform(wf)
geotransform_to_world(gt)
#>  xres yskew xskew  yres  xmin  ymax 
#>   1.0   0.0   0.0  -1.0   0.5  -0.5 
```
