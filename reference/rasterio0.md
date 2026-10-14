# GDAL RasterIO parameter creator

Basic function to create the window parameters as used by GDAL RasterIO.

## Usage

``` r
rasterio0(
  src_offset,
  src_dim,
  out_dim = src_dim,
  resample = "NearestNeighbour"
)
```

## Arguments

- src_offset:

  index offset (0-based, top left)

- src_dim:

  source dimension (XY)

- out_dim:

  output dimension (XY, optional src_dim will be used if not set)

- resample:

  resampling algorithm for GDAL see details

## Value

numeric vector of values specifying offset, source dimension, output
dimension

## Details

Resampling algorithm is one of 'NearestNeighbour' (default), 'Average',
'Bilinear', 'Cubic', 'CubicSpline', 'Gauss', 'Lanczos', 'Mode', but more
may be available given the version of GDAL in use.

## Examples

``` r
rasterio0(c(0L, 0L), src_dim = c(24L, 10L))
#>  offset_x  offset_y source_nx source_ny    out_nx    out_ny 
#>         0         0        24        10        24        10 
#> attr(,"resample")
#> [1] "NearestNeighbour"
```
