# The sf/stars RasterIO list

We create the list as used by the stars/sf GDAL IO function 'gdal_read(,
RasterIO_parameters)'.

## Usage

``` r
rasterio_to_sfio(x)
```

## Arguments

- x:

  rasterio params as from
  [`rasterio0()`](https://hypertidy.github.io/vaster/reference/rasterio0.md)

## Value

list in sf RasterIO format

## Details

Note that the input is a 4 or 6 element vector, with offset 0-based and
output dimensions optional (will use the source window). The resample
argument uses the syntax identical to that used in GDAL itself.

## Examples

``` r
rio <- rasterio0(c(0L, 0L), src_dim = c(24L, 10L))
rasterio_to_sfio(rio)
#> $nXOff
#> offset_x 
#>        1 
#> 
#> $nYOff
#> offset_y 
#>        1 
#> 
#> $nXSize
#> source_nx 
#>        24 
#> 
#> $nYSize
#> source_ny 
#>        10 
#> 
#> $nBufXSize
#> out_nx 
#>     24 
#> 
#> $nBufYSize
#> out_ny 
#>     10 
#> 
#> $resample
#> [1] "nearest_neighbour"
#> 
```
