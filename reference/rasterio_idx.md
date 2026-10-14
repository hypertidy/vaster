# RasterIO index window

The sf RasterIO is the RasterIO window in a list format used by the sf
package, it contains the same information, and is created by
`raster_sfio()`.

## Usage

``` r
rasterio_idx(dimension, extent)

raster_sfio(dimension, fact = 1, resample = "Nearest")
```

## Arguments

- dimension:

  ncols, nrows

- extent:

  this is ignored

- fact:

  a resizing factor

- resample:

  resample algorithm for GDAL RasterIO

## Value

RasterIO window vector 'c(x0, y0, nx0, ny0, nx, y)' see Details

## Examples

``` r
rasterio_idx(dim(volcano))
#>  offset_x  offset_y source_nx source_ny    out_nx    out_ny 
#>         0         0        87        61        87        61 
#> attr(,"resample")
#> [1] "NearestNeighbour"
```
