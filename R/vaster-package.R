#' vaster: Tools for Raster Grid Logic
#'
#' @description
#' Provides raster grid logic without requiring materialized data. Most raster
#' operations are purely functions of dimension (ncol, nrow) and extent (xmin,
#' xmax, ymin, ymax) - vaster provides these operations as simple functions on
#' numbers, independent of any file format or geospatial library.
#'
#' @section Core concepts:
#' A raster grid is defined by two properties:
#'
#' - **dimension**: `c(ncol, nrow)` - the number of columns and rows
#' - **extent**: `c(xmin, xmax, ymin, ymax)` - the bounding box
#'
#' From these six numbers, all grid geometry can be computed: cell indices,
#' coordinates, resolutions, and spatial queries.
#'
#' @section Cell indexing:
#' Cells are numbered from 1, starting at the top-left corner, proceeding
#' right then down (row-major order). This matches the convention used by
#' the raster and terra packages.
#'
#' @section Main function families:
#'
#' **Cell operations** ([cells]):
#' - `cell_from_xy()`, `cell_from_row_col()`, `cell_from_rowcol_combine()` - cell index from position
#' - `cell_from_row()`, `cell_from_col()` - all cells in a row or column
#' - `cell_from_extent()`, `extent_from_cell()` - cell/extent conversion
#' - `xy_from_cell()`, `x_from_cell()`, `y_from_cell()` - coordinates from cell
#' - `rowcol_from_cell()`, `row_from_cell()`, `col_from_cell()` - row/column from cell
#'
#' **Coordinate operations** ([coordinates]):
#' - `x_from_col()`, `y_from_row()` - coordinate from index
#' - `col_from_x()`, `row_from_y()` - index from coordinate
#' - `x_corner()`, `y_corner()` - corner coordinates of all cells
#' - `x_centre()`, `y_centre()` - centre coordinates of all cells
#' - `xy()` - centre coordinates of all cells as a matrix
#'
#' **Grid properties** ([grid]):
#' - `x_res()`, `y_res()` - cell resolution
#' - `n_cell()`, `n_row()`, `n_col()` - counts
#' - `xlim()`, `ylim()` - extent as 2-element ranges
#' - `x_min()`, `x_max()`, `y_min()`, `y_max()` - individual extent edges
#' - `origin()` - grid origin (alignment anchor)
#'
#' **Grid modification**:
#' - `vcrop()` - crop or extend a grid, snapped to alignment
#' - `align_extent()` - snap an extent to grid origin
#' - `extent_dimension()` - dimension for an aligned extent
#' - `intersect_extent()` - overlapping extent of two grids
#' - `snap_extent()` / `buffer_extent()` - snap extent to resolution
#'
#' **Cell adjacency** ([adjacency]):
#' - `adjacency()` - neighbour cell indices (queen, rook, or bishop)
#'
#' **GDAL interoperability**:
#' - `geo_transform0()`, `geo_world0()` - create geotransform / world file vectors
#' - `world_to_geotransform()`, `geotransform_to_world()` - convert between formats
#' - `extent_dim_to_gt()`, `gt_dim_to_extent()` - convert extent/dimension to/from geotransform
#' - `rasterio0()`, `rasterio_idx()`, `raster_sfio()` - GDAL RasterIO parameters
#' - `rasterio_to_sfio()`, `sfio_to_rasterio()` - convert between RasterIO formats
#' - `ts_te()`, `gdal_ts()`, `gdal_te()` - format dimension/extent for GDAL command line
#'
#' **Output and display**:
#' - `vaster_long()` - cell coordinates as long-form matrix
#' - `vaster_listxyz()` - grid as x, y, z list for [graphics::image()]
#' - `vaster_boundary()`, `vaster_boundary_cell()` - boundary coordinates
#' - `plot_extent()`, `draw_extent()` - plot and interactively draw extents
#' - `extent_vrt()` - tile extents from VRT files
#'
#' **Utilities**:
#' - `from_xyz()` - derive grid dimension and extent from XYZ points
#' - `fit_dims()` - compute dimension from aspect ratio and target size
#'
#' @section Extent convention:
#' vaster uses extent as `c(xmin, xmax, ymin, ymax)`, which differs from
#' the bbox convention `c(xmin, ymin, xmax, ymax)`. Both represent the same
#' information in different order.
#'
#' If extent is not provided, the default is `c(0, ncol, 0, nrow)` - matching
#' the convention adopted by stars, terra, and an improvement on base R's
#' `image()` which scales to 0-1.
#'
#' @aliases vaster-package
#' @useDynLib vaster, .registration = TRUE
"_PACKAGE"
