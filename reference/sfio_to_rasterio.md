# sf package RasterIO from RasterIO window vector

Basic function to create the window parameters as used by GDAL RasterIO,
in format used by sf, in 'gdal_read(,RasterIO_parameters)'.

## Usage

``` r
sfio_to_rasterio(x)
```

## Arguments

- x:

  a RasterIO parameter list

## Value

a sf-RasterIO parameter list

## Examples

``` r
sfio_to_rasterio(rasterio_to_sfio(rasterio0(c(0L, 0L), src_dim = c(24L, 10L))))
#>  offset_x  offset_y source_nx source_ny    out_nx    out_ny 
#>         0         0        24        10        24        10 
#> attr(,"resample")
#> [1] "nearest_neighbour"
```
