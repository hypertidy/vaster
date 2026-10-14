# Package index

## Package

- [`vaster`](https://hypertidy.github.io/vaster/reference/vaster-package.md)
  [`vaster-package`](https://hypertidy.github.io/vaster/reference/vaster-package.md)
  : vaster: Tools for Raster Grid Logic

## Grid structure

Dimension, extent, and grid origin.

- [`n_cell()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`x_res()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`y_res()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`n_row()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`n_col()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`xlim()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`ylim()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`x_min()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`x_max()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`y_min()`](https://hypertidy.github.io/vaster/reference/grid.md)
  [`y_max()`](https://hypertidy.github.io/vaster/reference/grid.md) :
  Grid
- [`origin()`](https://hypertidy.github.io/vaster/reference/origin.md) :
  Origin of grid alignment

## Cells & coordinates

Map between cell index, row/column, and position.

- [`cell_from_xy()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`cell_from_extent()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`extent_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`rowcol_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`xy_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`x_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`y_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`col_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`row_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`cell_from_row()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`cell_from_col()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`cell_from_row_col()`](https://hypertidy.github.io/vaster/reference/cells.md)
  [`cell_from_rowcol_combine()`](https://hypertidy.github.io/vaster/reference/cells.md)
  : Cells
- [`x_corner()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  [`y_corner()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  [`x_centre()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  [`y_centre()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  [`x_from_col()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  [`y_from_row()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  [`col_from_x()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  [`row_from_y()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  [`xy()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
  : Coordinates

## Extent alignment & cropping

Reconcile an arbitrary extent with a grid.

- [`align_extent()`](https://hypertidy.github.io/vaster/reference/align_extent.md)
  : Crop an extent, snapped to the grain
- [`snap_extent()`](https://hypertidy.github.io/vaster/reference/snap_extent.md)
  [`buffer_extent()`](https://hypertidy.github.io/vaster/reference/snap_extent.md)
  : Snap extent to resolution (buffer extent)
- [`vcrop()`](https://hypertidy.github.io/vaster/reference/vcrop.md) :
  Virtual grid modification
- [`intersect_extent()`](https://hypertidy.github.io/vaster/reference/intersect_extent.md)
  : Intersect extent
- [`extent_dimension()`](https://hypertidy.github.io/vaster/reference/extent_dimension.md)
  : Dimension for an aligned extent

## Lattice alignment & tiling

Grid-to-grid lattice relationships: exact index arithmetic between grids
that share a lattice. See vignette(“lattice-alignment”).

- [`is_aligned()`](https://hypertidy.github.io/vaster/reference/is_aligned.md)
  : Lattice alignment of two grids
- [`grid_offset()`](https://hypertidy.github.io/vaster/reference/grid_offset.md)
  : Integer offset between two grids of equal resolution
- [`snap_grid()`](https://hypertidy.github.io/vaster/reference/snap_grid.md)
  : Snap a grid onto a parent lattice
- [`resize_grid()`](https://hypertidy.github.io/vaster/reference/resize_grid.md)
  : Resize a grid by a factor, keeping the extent
- [`grid_tiles()`](https://hypertidy.github.io/vaster/reference/grid_tiles.md)
  : Partition a grid into tiles

## Grids from points

Derive or fit a grid specification.

- [`from_xyz()`](https://hypertidy.github.io/vaster/reference/from_xyz.md)
  : Derive a grid from XYZ points
- [`fit_dims()`](https://hypertidy.github.io/vaster/reference/fit_dims.md)
  : Aspect ratio of dimension conflated with bbox

## Neighbourhoods

- [`adjacency()`](https://hypertidy.github.io/vaster/reference/adjacency.md)
  : Cell adjacency

## GDAL & geotransform interop

Geotransforms, VRT extents, and RasterIO windows.

- [`ts_te()`](https://hypertidy.github.io/vaster/reference/ts_te.md)
  [`gdal_te()`](https://hypertidy.github.io/vaster/reference/ts_te.md)
  [`gdal_ts()`](https://hypertidy.github.io/vaster/reference/ts_te.md) :
  Target size and extent for GDAL command line
- [`extent_vrt()`](https://hypertidy.github.io/vaster/reference/extent_vrt.md)
  : Extents from VRT (virtual raster data set of GDAL)
- [`geo_transform0()`](https://hypertidy.github.io/vaster/reference/geo_transform0.md)
  : Geo transform parameter creator
- [`geo_world0()`](https://hypertidy.github.io/vaster/reference/geo_world0.md)
  [`geotransform_to_world()`](https://hypertidy.github.io/vaster/reference/geo_world0.md)
  : World file parameter creator
- [`world_to_geotransform()`](https://hypertidy.github.io/vaster/reference/world_to_geotransform.md)
  : Create geotransform from world vector
- [`extent_dim_to_gt()`](https://hypertidy.github.io/vaster/reference/extent_dim_to_gt.md)
  : Create geotransform from extent and dimension
- [`gt_dim_to_extent()`](https://hypertidy.github.io/vaster/reference/gt_dim_to_extent.md)
  : Determine extent from geotransform vector and dimension
- [`rasterio0()`](https://hypertidy.github.io/vaster/reference/rasterio0.md)
  : GDAL RasterIO parameter creator
- [`rasterio_idx()`](https://hypertidy.github.io/vaster/reference/rasterio_idx.md)
  [`raster_sfio()`](https://hypertidy.github.io/vaster/reference/rasterio_idx.md)
  : RasterIO index window
- [`rasterio_to_sfio()`](https://hypertidy.github.io/vaster/reference/rasterio_to_sfio.md)
  : The sf/stars RasterIO list
- [`sfio_to_rasterio()`](https://hypertidy.github.io/vaster/reference/sfio_to_rasterio.md)
  : sf package RasterIO from RasterIO window vector

## Long-form & boundary conversions

- [`vaster_long()`](https://hypertidy.github.io/vaster/reference/vaster_long.md)
  : Convert to long form coordinates
- [`vaster_listxyz()`](https://hypertidy.github.io/vaster/reference/vaster_listxyz.md)
  : Image xyz list
- [`vaster_boundary()`](https://hypertidy.github.io/vaster/reference/vaster_boundary.md)
  : Grid boundary in native resolution
- [`vaster_boundary_cell()`](https://hypertidy.github.io/vaster/reference/vaster_boundary_cell.md)
  : Grid boundary cell index

## Plotting

- [`plot_extent()`](https://hypertidy.github.io/vaster/reference/plot_extent.md)
  : Plot an extent
- [`draw_extent()`](https://hypertidy.github.io/vaster/reference/draw_extent.md)
  : Draw extent
