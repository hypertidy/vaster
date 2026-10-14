# Extents from VRT (virtual raster data set of GDAL)

Get extent from index values in VRT text, these are the individual
footprints of raster windows in a VRT. These can be arbitrary to a
specific grid, but generally are used for tiled mosaics.

## Usage

``` r
extent_vrt(x)
```

## Arguments

- x:

  url or file path to VRT file

## Value

a matrix of 4 columns, xmin,xmax,ymin,ymax

## Details

Each VRT raster element records it's relative position within the grid,
so we grid logic to expand the actual extent of each element, and return
those as a matrix of xmin,xmax,ymin,ymax.

## Examples

``` r
#src <- "https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt"
src <- gzfile(system.file("extdata/NASADEM_be.vrt.gz", package = "vaster"), "rt")
## read VRT from a URL or file (we use a connection here to keep package example small)
ex <- extent_vrt(src)
close(src)
plot_extent(ex)
```
