## Lattice alignment: the "alignment prison"
##
## Two grids are compatible if and only if their cell boundaries coincide:
##   1. commensurate resolution (one is an exact integer multiple of the other)
##   2. same origin modulo the finer resolution
## When these hold every index operation (offset, resize, tile, reindex)
## produces exact integer results and no floating-point negotiation is
## needed. When they do not hold these functions refuse to proceed.
##
## Tolerance policy: one relative tolerance, lattice_tol(), used everywhere in
## this file. A quantity q that should be an integer is accepted when
## abs(q - round(q)) <= lattice_tol() * max(1, abs(q)).
##
## See inst/design-docs/align-design.md for the full rationale.

lattice_tol <- function() sqrt(.Machine$double.eps)

.is_whole <- function(q, tol = lattice_tol()) {
  abs(q - round(q)) <= tol * pmax(1, abs(q))
}

.res <- function(dimension, extent) {
  c(x_res(dimension, extent), y_res(dimension, extent))
}

#' Lattice alignment of two grids
#'
#' Two grids share a lattice when their cell boundaries coincide: the
#' resolutions are commensurate (one is an exact integer multiple of the
#' other, in x and in y) and the origins differ by a whole number of the
#' finer cells. When this holds every index operation between the grids
#' is exact integer arithmetic. The test is symmetric.
#'
#' Note that alignment is not containment: a grid may be aligned to another
#' and lie entirely outside it. Use [intersect_extent()] for overlap.
#'
#' @inheritParams grid
#' @param dimension2 dimension of the second grid (ncol, nrow)
#' @param extent2 extent of the second grid (xmin, xmax, ymin, ymax),
#'   `NULL` for unit cell space
#' @param tol relative tolerance for whole-number tests
#' @return logical
#' @export
#' @seealso [grid_offset()], [snap_grid()], [resize_grid()], [grid_tiles()]
#' @examples
#' is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(2, 7, 2, 7))
#' is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(2.5, 7.5, 2, 7))
#' ## coarser grid on the same lattice
#' is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(2L, 2L), c(0, 10, 0, 10))
is_aligned <- function(dimension, extent = NULL, dimension2, extent2 = NULL,
                       tol = lattice_tol()) {
  extent <- extent %||% extent0(dimension)
  extent2 <- extent2 %||% extent0(dimension2)
  .check_args(dimension, extent)
  .check_args(dimension2, extent2)

  r1 <- .res(dimension, extent)
  r2 <- .res(dimension2, extent2)
  rmin <- pmin(r1, r2)

  ## commensurate: each resolution is a whole multiple of the finer
  if (!all(.is_whole(r1 / rmin, tol))) return(FALSE)
  if (!all(.is_whole(r2 / rmin, tol))) return(FALSE)

  ## origin: offset between origins is a whole number of finer cells
  ## (x anchored at xmin, y anchored at ymax: the raster top-left origin)
  off <- c((extent2[1L] - extent[1L]) / rmin[1L],
           (extent[4L] - extent2[4L]) / rmin[2L])
  all(.is_whole(off, tol))
}

#' Integer offset between two grids of equal resolution
#'
#' The column and row offset from the top-left cell of the parent grid to
#' the top-left cell of the child grid. Both grids must have the same
#' resolution and share a lattice. The offset may be negative or exceed
#' the parent dimension: alignment is not containment.
#'
#' @inheritParams is_aligned
#' @return named integer vector `c(col = , row = )`, 0-based
#' @export
#' @examples
#' ## child starts 2 cells in and 3 cells down
#' grid_offset(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(2, 7, 2, 7))
grid_offset <- function(dimension, extent = NULL, dimension2, extent2 = NULL,
                        tol = lattice_tol()) {
  extent <- extent %||% extent0(dimension)
  extent2 <- extent2 %||% extent0(dimension2)
  .check_args(dimension, extent)
  .check_args(dimension2, extent2)

  r1 <- .res(dimension, extent)
  r2 <- .res(dimension2, extent2)
  if (any(abs(r1 - r2) > tol * pmax(1, abs(r1)))) {
    stop("grid_offset requires identical resolution; use snap_grid() or resize_grid() first")
  }
  off <- c((extent2[1L] - extent[1L]) / r1[1L],
           (extent[4L] - extent2[4L]) / r1[2L])
  if (!all(.is_whole(off, tol))) {
    stop("grids are not aligned (origins differ by a fractional cell)")
  }
  c(col = as.integer(round(off[1L])), row = as.integer(round(off[2L])))
}

#' Snap a grid onto a parent lattice
#'
#' Moves the extent of the first grid so that its cell boundaries fall on
#' cell boundaries of the parent grid, keeping its own resolution. The two
#' resolutions must be commensurate. The snap is to the coarser of the two
#' resolutions, anchored at the parent's top-left origin, so the result is
#' always aligned to the parent (and the parent to it).
#'
#' This differs from [align_extent()] / [vcrop()], which snap an arbitrary
#' extent to a grid and take that grid's resolution. Here the grid being
#' snapped keeps its resolution and only its origin moves.
#'
#' @param dimension dimension of the grid to snap (ncol, nrow)
#' @param extent extent of the grid to snap
#' @param dimension2 dimension of the parent grid
#' @param extent2 extent of the parent grid
#' @param tol relative tolerance for whole-number tests
#' @return list with `dimension` (unchanged) and `extent` (snapped)
#' @export
#' @examples
#' ## 2x2 grid at res 2, slightly off a unit lattice
#' snap_grid(c(2L, 2L), c(0.4, 4.4, 0.4, 4.4), c(10L, 10L), c(0, 10, 0, 10))
snap_grid <- function(dimension, extent = NULL, dimension2, extent2 = NULL,
                      tol = lattice_tol()) {
  extent <- extent %||% extent0(dimension)
  extent2 <- extent2 %||% extent0(dimension2)
  .check_args(dimension, extent)
  .check_args(dimension2, extent2)

  rx <- .res(dimension, extent)
  rp <- .res(dimension2, extent2)
  rmin <- pmin(rx, rp)
  if (!all(.is_whole(rx / rmin, tol)) || !all(.is_whole(rp / rmin, tol))) {
    stop("resolutions are not commensurate; snap_grid requires integer-related resolutions")
  }
  ## snap to the coarser lattice: it is a sublattice of the finer one, so
  ## the result is aligned whichever grid is finer
  rs <- pmax(rx, rp)
  ## anchor at the parent top-left (xmin, ymax)
  x0 <- extent2[1L] + round((extent[1L] - extent2[1L]) / rs[1L]) * rs[1L]
  y1 <- extent2[4L] - round((extent2[4L] - extent[4L]) / rs[2L]) * rs[2L]
  new_extent <- c(x0, x0 + dimension[1L] * rx[1L],
                  y1 - dimension[2L] * rx[2L], y1)
  list(dimension = as.integer(dimension), extent = new_extent)
}

#' Resize a grid by a factor, keeping the extent
#'
#' Change resolution by a factor that yields whole-number dimensions. The
#' new lattice nests exactly within the old one (factor > 1) or the old
#' within the new (factor < 1), so `resize_grid(resize_grid(g, k), 1/k)`
#' returns `g`.
#'
#' @inheritParams grid
#' @param factor positive scaling factor, applied to the dimension; a single
#'   value or `c(xfactor, yfactor)`
#' @param tol relative tolerance for whole-number tests
#' @return list with `dimension` and `extent`
#' @export
#' @examples
#' resize_grid(c(10L, 10L), c(0, 10, 0, 10), 2)
#' resize_grid(c(10L, 10L), c(0, 10, 0, 10), 0.5)
#' try(resize_grid(c(10L, 10L), c(0, 10, 0, 10), 0.3))
resize_grid <- function(dimension, extent = NULL, factor, tol = lattice_tol()) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  factor <- rep_len(as.double(factor), 2L)
  if (any(!is.finite(factor)) || any(factor <= 0)) stop("factor must be positive")
  nd <- dimension * factor
  if (!all(.is_whole(nd, tol))) {
    stop("factor does not produce whole-number dimensions")
  }
  nd <- as.integer(round(nd))
  if (any(nd < 1L)) stop("resize produces zero dimension")
  list(dimension = nd, extent = as.double(extent))
}

#' Partition a grid into tiles
#'
#' Pure index arithmetic: how many tiles fit and what their extents are.
#' Tiles are enumerated left to right, then top to bottom, matching cell
#' order. Edge tiles may be smaller than `tile_dim`. Every tile is aligned
#' to the parent and [grid_offset()] of a tile is its origin in parent
#' cells.
#'
#' @inheritParams grid
#' @param tile_dim tile dimension (ncol, nrow), recycled if length 1
#' @return list of lists, each with `dimension` and `extent`, plus
#'   `offset` (the tile's 0-based `c(col, row)` origin in the parent)
#' @export
#' @examples
#' tl <- grid_tiles(c(10L, 7L), c(0, 10, 0, 7), c(4L, 4L))
#' length(tl)
#' tl[[6]]
grid_tiles <- function(dimension, extent = NULL, tile_dim = c(256L, 256L)) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  tile_dim <- as.integer(rep_len(tile_dim, 2L))
  if (any(tile_dim < 1L)) stop("tile_dim must be >= 1")

  r <- .res(dimension, extent)
  nx <- ceiling(dimension[1L] / tile_dim[1L])
  ny <- ceiling(dimension[2L] / tile_dim[2L])
  out <- vector("list", nx * ny)
  k <- 0L
  for (j in seq_len(ny)) {
    for (i in seq_len(nx)) {
      col0 <- (i - 1L) * tile_dim[1L]
      row0 <- (j - 1L) * tile_dim[2L]
      nc <- min(tile_dim[1L], dimension[1L] - col0)
      nr <- min(tile_dim[2L], dimension[2L] - row0)
      k <- k + 1L
      out[[k]] <- list(
        dimension = as.integer(c(nc, nr)),
        extent = c(extent[1L] + col0 * r[1L],
                   extent[1L] + (col0 + nc) * r[1L],
                   extent[4L] - (row0 + nr) * r[2L],
                   extent[4L] - row0 * r[2L]),
        offset = c(col = as.integer(col0), row = as.integer(row0))
      )
    }
  }
  out
}
