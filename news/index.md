# Changelog

## vaster 0.7.0

- New lattice-alignment functions, folded in from the mdsumner/align
  sketch:
  [`is_aligned()`](https://hypertidy.github.io/vaster/reference/is_aligned.md),
  [`grid_offset()`](https://hypertidy.github.io/vaster/reference/grid_offset.md),
  [`snap_grid()`](https://hypertidy.github.io/vaster/reference/snap_grid.md),
  [`resize_grid()`](https://hypertidy.github.io/vaster/reference/resize_grid.md)
  and
  [`grid_tiles()`](https://hypertidy.github.io/vaster/reference/grid_tiles.md).
  Two grids are aligned when their resolutions are commensurate and
  their origins differ by a whole number of the finer cells; the
  functions refuse to proceed otherwise, so index arithmetic between
  aligned grids is exact. One tolerance policy
  (`sqrt(.Machine$double.eps)`, relative) is used throughout. The align
  design doc is at `inst/design-docs/align-design.md`, and these
  functions are documented in the new “Lattice Alignment & Tiling”
  vignette.

- Fixed:
  [`extent_vrt()`](https://hypertidy.github.io/vaster/reference/extent_vrt.md)
  took ymin from the first (top) row and ymax from the last row, so ymin
  \> ymax for every tile, and used cell centres as edges, so each tile
  was half a cell short on every side. The bundled NASADEM example hid
  the second problem (3601 px tiles with a one-pixel overlap put the end
  centres on whole degrees) and
  [`plot_extent()`](https://hypertidy.github.io/vaster/reference/plot_extent.md)/[`rect()`](https://rdrr.io/r/graphics/rect.html)
  is indifferent to corner order. Now uses the end-cell centres pushed
  out by half a cell, with y taken the right way round.

## vaster 0.6.0

CRAN release: 2026-03-10

- New function
  [`adjacency()`](https://hypertidy.github.io/vaster/reference/adjacency.md)
  for cell-based neighbourhood lookup, replacing the old matrix shift
  functions (`tl()`, `bl()`, `tr()`, `br()`, `la()`, `ta()`, `ra()`,
  `ba()`). The new function takes `dimension` and `cell` arguments
  consistent with the rest of vaster, returning a matrix of neighbour
  cell indices with named columns for direction. Supports `"queen"`,
  `"rook"`, and `"bishop"` neighbourhood types.

- Renamed [`ts()`](https://rdrr.io/r/stats/ts.html) and `te()` to
  [`gdal_ts()`](https://hypertidy.github.io/vaster/reference/ts_te.md)
  and
  [`gdal_te()`](https://hypertidy.github.io/vaster/reference/ts_te.md)
  to avoid masking [`stats::ts()`](https://rdrr.io/r/stats/ts.html).

## vaster 0.5.0

- We now have internal functions `col_from_x_c()` and `row_from_y_c()`,
  `x_from_col_c()` and `y_from_row_c()` that use C under the hood, these
  currently accept and return 0-based indexes.

- Package now requires compilation, with the R C api.

## vaster 0.0.2

- Speed up `x_from_col` and `y_from_row`, fixes
  [\#19](https://github.com/hypertidy/vaster/issues/19).

- New functions
  [`rasterio_idx()`](https://hypertidy.github.io/vaster/reference/rasterio_idx.md)
  (was affinity::raster_to_rasterio), `rasterio0` (was
  affinity::raster_io0) and internal for now `rasterio_crop()`.

differences are - no raster objects - arguments dimension, extent as
needed - there’s some overlap with vcrop and so on and needs checking -
the sf RasterIO stuff now works well with {ximage} - fact and dimension
need some thought for how the ux should be

## vaster 0.0.1

- New functions `tl()`, `ta()`, `tr()`, `ra()`, `br()`, `ba()`. `br()`,
  `la()` which are top left, top, top right, right, bottom right,
  bottom, bottom right, left. These are different from the affinity ones
  because they don’t keep the extra rows and columns (the variants in
  affinity might become options with these). The ‘a’ means adjacent, we
  can’t use t(), r(), l(), b() and top, right, left, bottom seems weird.

- New function
  [`from_xyz()`](https://hypertidy.github.io/vaster/reference/from_xyz.md)
  to return a grid (dimension and extent) derived from “XYZ” points,
  basically are the points (in whatever order, and may be incomplete) on
  a regular grid within the rounding threshold of the ‘digits’
  parameter.

- New functions [`ts()`](https://rdrr.io/r/stats/ts.html), `te()`, and
  [`ts_te()`](https://hypertidy.github.io/vaster/reference/ts_te.md) to
  format dimension,extent for use with GDAL options.

- [`plot_extent()`](https://hypertidy.github.io/vaster/reference/plot_extent.md)
  is now vectorized, just give it xmin, xmax, ymin, ymax in that order
  in vector, matrix, or data frame.

- New function
  [`snap_extent()`](https://hypertidy.github.io/vaster/reference/snap_extent.md)
  to replace `spex::buffer_extent()`, aliased to
  [`buffer_extent()`](https://hypertidy.github.io/vaster/reference/snap_extent.md)
  in this package.

- Better argument consistency, dimension first, then extent.

- New function
  [`extent_vrt()`](https://hypertidy.github.io/vaster/reference/extent_vrt.md)
  to obtain tile extents from VRT text.

- New function
  [`vaster_listxyz()`](https://hypertidy.github.io/vaster/reference/vaster_listxyz.md)
  to convert extent, dimension, and optionally data to trad
  [`image()`](https://rdrr.io/r/graphics/image.html) format.

- New function
  [`vaster_long()`](https://hypertidy.github.io/vaster/reference/vaster_long.md)
  to convert extent, dimension, and optionally data to long form XYZ in
  raster or trad matrix order.

- All obvious raster functions now included (needs more testing).

- Version bump to allow {grout} to depend on vaster for cell and extent
  ops.

- Added a `NEWS.md` file to track changes to the package.
