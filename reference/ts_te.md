# Target size and extent for GDAL command line

Format grid properties for GDAL command line options (`-ts` for target
size, `-te` for target extent).

## Usage

``` r
ts_te(dimension, extent)

gdal_te(extent)

gdal_ts(dimension)
```

## Arguments

- dimension:

  integer vector of ncol, nrow (target size)

- extent:

  numeric vector of xmin, xmax, ymin, ymax (target extent)

## Value

A character string formatted for GDAL command line:

- `ts_te()`: combined `-ts` and `-te` arguments

- `gdal_ts()`: `-ts ncol nrow` string

- `gdal_te()`: `-te xmin ymin xmax ymax` string (note: reordered for
  GDAL)

## Details

These functions generate the string arguments used by GDAL utilities
like `gdalwarp` and `gdal_translate`. The `gdal_ts()` function is named
after the GDAL `-ts` flag and `gdal_te()` after the GDAL `-te` flag.

## See also

[`vcrop()`](https://hypertidy.github.io/vaster/reference/vcrop.md) for
computing aligned extents

## Examples

``` r
ts_te(c(10, 100), 1:4)
#> [1] "-ts 10 100 -te 1 3 2 4"

gdal_ts(c(10, 100))
#> [1] "-ts 10 100"

gdal_te(1:4)
#> [1] "-te 1 3 2 4"

## use in a GDAL command (not run)
if (FALSE) { # \dontrun{
cmd <- sprintf("gdalwarp %s %s input.tif output.tif",
               gdal_ts(c(1000, 500)), gdal_te(c(-180, 180, -90, 90)))
} # }
```
