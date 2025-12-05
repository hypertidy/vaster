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
#' - `cell_from_xy()`, `cell_from_row_col()` - get cell index from position
#' - `xy_from_cell()`, `rowcol_from_cell()` - get position from cell index
#' - `row_from_cell()`, `col_from_cell()` - decompose cell to row/column
#'
#' **Coordinate operations** ([coordinates]):
#' - `x_from_col()`, `y_from_row()` - coordinate from index
#' - `col_from_x()`, `row_from_y()` - index from coordinate
#' - `x_corner()`, `y_corner()` - corner coordinates
#' - `x_centre()`, `y_centre()` - cell centre coordinates
#'
#' **Grid properties** ([grid]):
#' - `x_res()`, `y_res()` - cell resolution
#' - `n_cell()`, `n_row()`, `n_col()` - counts
#' - `xlim()`, `ylim()` - extent components
#'
#' **Grid modification**:
#' - [vcrop()] - crop/extend a grid, snapped to alignment
#' - [align_extent()] - snap an extent to grid origin
#'
#' **Adjacency operations** ([adjacencies]):
#' - `tl()`, `tr()`, `bl()`, `br()` - corner adjacencies
#' - `ta()`, `ba()`, `la()`, `ra()` - edge adjacencies
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
#' @section Note on stats::ts:
#' This package exports a function `ts()` for GDAL command line formatting,
#' which masks [stats::ts()]. Use `stats::ts()` explicitly if you need time
#' series functionality, or use [ts_te()] instead.
#'
#' @keywords internal
#' @aliases vaster-package
#' @useDynLib vaster, .registration = TRUE
"_PACKAGE"
