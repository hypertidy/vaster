# vaster 0.0.2

* Speed up `x_from_col` and `y_from_row`, fixes #19. 

* New functions `rasterio_idx()` (was affinity::raster_to_rasterio), `rasterio0` (was affinity::raster_io0) and
internal for now `rasterio_crop()`. 

differences are
- no raster objects
- arguments dimension, extent as needed
- there's some overlap with vcrop and so on and needs checking
- the sf RasterIO stuff now works well with {ximage}
- fact and dimension need some thought for how the ux should be

# vaster 0.0.1

* New functions `tl()`, `ta()`, `tr()`, `ra()`, `br()`, `ba()`. `br()`, `la()`
which are top left, top, top right, right, bottom right, bottom, bottom right,
left. These are different from the affinity ones because they don't keep the
extra rows and columns (the variants in affinity might become options with
these). The 'a' means adjacent, we can't use t(), r(), l(), b() and top, right,
left, bottom seems weird.

* New function `from_xyz()` to return a grid (dimension and extent) derived from
"XYZ" points, basically are the points (in whatever order, and may be
incomplete) on a regular grid within the rounding threshold of the 'digits'
parameter.

* New functions `ts()`, `te()`, and `ts_te()` to format dimension,extent for use
with GDAL options.

* `plot_extent()` is now vectorized, just give it xmin, xmax, ymin, ymax in that
order in vector, matrix, or data frame.

* New function `snap_extent()` to replace `spex::buffer_extent()`, aliased to
`buffer_extent()` in this package.

* Better argument consistency, dimension first, then extent. 

* New function `extent_vrt()` to obtain tile extents from VRT text. 

* New function `vaster_listxyz()` to convert extent, dimension, and optionally
data to trad `image()` format.

* New function `vaster_long()` to convert extent, dimension, and optionally data
to long form XYZ in raster or trad matrix order.

* All obvious raster functions now included (needs more testing). 

* Version bump to allow {grout} to depend on vaster for cell and extent ops. 

* Added a `NEWS.md` file to track changes to the package.
