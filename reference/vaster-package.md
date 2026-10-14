# vaster: Tools for Raster Grid Logic

Provides raster grid logic without requiring materialized data. Most
raster operations are purely functions of dimension (ncol, nrow) and
extent (xmin, xmax, ymin, ymax) - vaster provides these operations as
simple functions on numbers, independent of any file format or
geospatial library.

## Core concepts

A raster grid is defined by two properties:

- **dimension**: `c(ncol, nrow)` - the number of columns and rows

- **extent**: `c(xmin, xmax, ymin, ymax)` - the bounding box

From these six numbers, all grid geometry can be computed: cell indices,
coordinates, resolutions, and spatial queries.

## Cell indexing

Cells are numbered from 1, starting at the top-left corner, proceeding
right then down (row-major order). This matches the convention used by
the raster and terra packages.

## Main function families

**Cell operations**
([cells](https://hypertidy.github.io/vaster/reference/cells.md)):

- [`cell_from_xy()`](https://hypertidy.github.io/vaster/reference/cells.md),
  [`cell_from_row_col()`](https://hypertidy.github.io/vaster/reference/cells.md),
  [`cell_from_rowcol_combine()`](https://hypertidy.github.io/vaster/reference/cells.md) -
  cell index from position

- [`cell_from_row()`](https://hypertidy.github.io/vaster/reference/cells.md),
  [`cell_from_col()`](https://hypertidy.github.io/vaster/reference/cells.md) -
  all cells in a row or column

- [`cell_from_extent()`](https://hypertidy.github.io/vaster/reference/cells.md),
  [`extent_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md) -
  cell/extent conversion

- [`xy_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md),
  [`x_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md),
  [`y_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md) -
  coordinates from cell

- [`rowcol_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md),
  [`row_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md),
  [`col_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md) -
  row/column from cell

**Coordinate operations**
([coordinates](https://hypertidy.github.io/vaster/reference/coordinates.md)):

- [`x_from_col()`](https://hypertidy.github.io/vaster/reference/coordinates.md),
  [`y_from_row()`](https://hypertidy.github.io/vaster/reference/coordinates.md) -
  coordinate from index

- [`col_from_x()`](https://hypertidy.github.io/vaster/reference/coordinates.md),
  [`row_from_y()`](https://hypertidy.github.io/vaster/reference/coordinates.md) -
  index from coordinate

- [`x_corner()`](https://hypertidy.github.io/vaster/reference/coordinates.md),
  [`y_corner()`](https://hypertidy.github.io/vaster/reference/coordinates.md) -
  corner coordinates of all cells

- [`x_centre()`](https://hypertidy.github.io/vaster/reference/coordinates.md),
  [`y_centre()`](https://hypertidy.github.io/vaster/reference/coordinates.md) -
  centre coordinates of all cells

- [`xy()`](https://hypertidy.github.io/vaster/reference/coordinates.md) -
  centre coordinates of all cells as a matrix

**Grid properties**
([grid](https://hypertidy.github.io/vaster/reference/grid.md)):

- [`x_res()`](https://hypertidy.github.io/vaster/reference/grid.md),
  [`y_res()`](https://hypertidy.github.io/vaster/reference/grid.md) -
  cell resolution

- [`n_cell()`](https://hypertidy.github.io/vaster/reference/grid.md),
  [`n_row()`](https://hypertidy.github.io/vaster/reference/grid.md),
  [`n_col()`](https://hypertidy.github.io/vaster/reference/grid.md) -
  counts

- [`xlim()`](https://hypertidy.github.io/vaster/reference/grid.md),
  [`ylim()`](https://hypertidy.github.io/vaster/reference/grid.md) -
  extent as 2-element ranges

- [`x_min()`](https://hypertidy.github.io/vaster/reference/grid.md),
  [`x_max()`](https://hypertidy.github.io/vaster/reference/grid.md),
  [`y_min()`](https://hypertidy.github.io/vaster/reference/grid.md),
  [`y_max()`](https://hypertidy.github.io/vaster/reference/grid.md) -
  individual extent edges

- [`origin()`](https://hypertidy.github.io/vaster/reference/origin.md) -
  grid origin (alignment anchor)

**Grid modification**:

- [`vcrop()`](https://hypertidy.github.io/vaster/reference/vcrop.md) -
  crop or extend a grid, snapped to alignment

- [`align_extent()`](https://hypertidy.github.io/vaster/reference/align_extent.md) -
  snap an extent to grid origin

- [`extent_dimension()`](https://hypertidy.github.io/vaster/reference/extent_dimension.md) -
  dimension for an aligned extent

- [`intersect_extent()`](https://hypertidy.github.io/vaster/reference/intersect_extent.md) -
  overlapping extent of two grids

- [`snap_extent()`](https://hypertidy.github.io/vaster/reference/snap_extent.md)
  /
  [`buffer_extent()`](https://hypertidy.github.io/vaster/reference/snap_extent.md) -
  snap extent to resolution

**Cell adjacency**
([adjacency](https://hypertidy.github.io/vaster/reference/adjacency.md)):

- [`adjacency()`](https://hypertidy.github.io/vaster/reference/adjacency.md) -
  neighbour cell indices (queen, rook, or bishop)

**GDAL interoperability**:

- [`geo_transform0()`](https://hypertidy.github.io/vaster/reference/geo_transform0.md),
  [`geo_world0()`](https://hypertidy.github.io/vaster/reference/geo_world0.md) -
  create geotransform / world file vectors

- [`world_to_geotransform()`](https://hypertidy.github.io/vaster/reference/world_to_geotransform.md),
  [`geotransform_to_world()`](https://hypertidy.github.io/vaster/reference/geo_world0.md) -
  convert between formats

- [`extent_dim_to_gt()`](https://hypertidy.github.io/vaster/reference/extent_dim_to_gt.md),
  [`gt_dim_to_extent()`](https://hypertidy.github.io/vaster/reference/gt_dim_to_extent.md) -
  convert extent/dimension to/from geotransform

- [`rasterio0()`](https://hypertidy.github.io/vaster/reference/rasterio0.md),
  [`rasterio_idx()`](https://hypertidy.github.io/vaster/reference/rasterio_idx.md),
  [`raster_sfio()`](https://hypertidy.github.io/vaster/reference/rasterio_idx.md) -
  GDAL RasterIO parameters

- [`rasterio_to_sfio()`](https://hypertidy.github.io/vaster/reference/rasterio_to_sfio.md),
  [`sfio_to_rasterio()`](https://hypertidy.github.io/vaster/reference/sfio_to_rasterio.md) -
  convert between RasterIO formats

- [`ts_te()`](https://hypertidy.github.io/vaster/reference/ts_te.md),
  [`gdal_ts()`](https://hypertidy.github.io/vaster/reference/ts_te.md),
  [`gdal_te()`](https://hypertidy.github.io/vaster/reference/ts_te.md) -
  format dimension/extent for GDAL command line

**Output and display**:

- [`vaster_long()`](https://hypertidy.github.io/vaster/reference/vaster_long.md) -
  cell coordinates as long-form matrix

- [`vaster_listxyz()`](https://hypertidy.github.io/vaster/reference/vaster_listxyz.md) -
  grid as x, y, z list for
  [`graphics::image()`](https://rdrr.io/r/graphics/image.html)

- [`vaster_boundary()`](https://hypertidy.github.io/vaster/reference/vaster_boundary.md),
  [`vaster_boundary_cell()`](https://hypertidy.github.io/vaster/reference/vaster_boundary_cell.md) -
  boundary coordinates

- [`plot_extent()`](https://hypertidy.github.io/vaster/reference/plot_extent.md),
  [`draw_extent()`](https://hypertidy.github.io/vaster/reference/draw_extent.md) -
  plot and interactively draw extents

- [`extent_vrt()`](https://hypertidy.github.io/vaster/reference/extent_vrt.md) -
  tile extents from VRT files

**Utilities**:

- [`from_xyz()`](https://hypertidy.github.io/vaster/reference/from_xyz.md) -
  derive grid dimension and extent from XYZ points

- [`fit_dims()`](https://hypertidy.github.io/vaster/reference/fit_dims.md) -
  compute dimension from aspect ratio and target size

## Extent convention

vaster uses extent as `c(xmin, xmax, ymin, ymax)`, which differs from
the bbox convention `c(xmin, ymin, xmax, ymax)`. Both represent the same
information in different order.

If extent is not provided, the default is `c(0, ncol, 0, nrow)` -
matching the convention adopted by stars, terra, and an improvement on
base R's [`image()`](https://rdrr.io/r/graphics/image.html) which scales
to 0-1.

## See also

Useful links:

- <https://github.com/hypertidy/vaster>

- <https://hypertidy.github.io/vaster/>

- Report bugs at <https://github.com/hypertidy/vaster/issues>

## Author

**Maintainer**: Michael Sumner <mdsumner@gmail.com>
([ORCID](https://orcid.org/0000-0002-2471-7511)) \[copyright holder\]

Authors:

- Michael Sumner <mdsumner@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-2471-7511)) \[copyright holder\]

Other contributors:

- Robert Hijmans (Wrote original versions of abstract cell operations in
  raster package) \[contributor\]
