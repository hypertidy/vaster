#' Snap extent to resolution (buffer extent)
#'
#' Whole grain buffers.
#'
#' @param x extent (xmin, xmax, ymin, ymax)
#' @param res resolution (a grain to align to)
#'
#' @return extent, snapped to the resolution
#' @export
#'
#' @examples
#' snap_extent(sort(rnorm(4)), 0.01)
snap_extent <-  function(x, res) {
  if (missing(res) || length(res) < 1 || is.null(res) || res[1L] == 0 || is.na(res[1L])) {
    return(x)
  }
  (x %/% res) * res + c(0, res, 0, res)
}

#' @name snap_extent
#' @export
#' @aliases snap_extent
buffer_extent <- function(x, res) {
  snap_extent(x, res)
}


#' Intersect extent
#'
#' Return the overlapping extent.
#'
#' @param x extent to intersect
#' @inheritParams grid
#'
#' @return extent
#' @export
#'
#' @examples
#' intersect_extent(c(0.5, 2.3, 1.2, 5), c(10, 5), c(0, 10, 0, 5))
intersect_extent <- function(x, dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension)
  vcrop(x, dimension, extent)$extent
}



#' Dimension for an aligned extent
#'
#' input is the output of align_extent
#'
#'
#' @param x and aligned extent
#' @param dimension dimension of parent
#' @param extent of parent
#' @param snap out by default, may be near or in
#' @return dimension
#' @export
#'
#' @examples
#' extent_dimension(c(.2, .8, 1.8, 3.2), c(10, 5), c(0, 10, 0, 5))
extent_dimension <- function(x, dimension, extent = NULL, snap = "out") {
  extent <- extent %||% extent0(dimension)
  ## avoid truncation by rounding
  # as.integer(round(c(diff(x[1:2]) / x_res(dimension, extent),
  #   diff(x[3:4]) / y_res(dimension, extent))))
  ex <- align_extent(x, dimension, extent, snap = snap)
  as.integer(round(c(diff(ex[1:2]) / x_res(dimension, extent),
                     diff(ex[3:4]) / y_res(dimension, extent))))

  # as.integer(ceiling(c(diff(ex[1:2]) / x_res(dimension, extent),
  #                    diff(ex[3:4]) / y_res(dimension, extent))))
  #
  # as.integer((c(diff(ex[1:2]) / x_res(dimension, extent),
  #                      diff(ex[3:4]) / y_res(dimension, extent))))
}


#' @aliases extent_dimension
e_dim <- extent_dimension

#' Origin of grid alignment
#'
#' @inheritParams grid
#'
#' @return coordinate of grid origin
#' @export
#'
#' @examples
#' origin(c(10, 5), c(0, 10, 0, 5))
origin <-   function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension)
  r <- c(x_res(dimension, extent), y_res(dimension, extent))
  x <- extent[1L] - r[1]*(round(extent[1L] / r[1]))
  y <- extent[4L] - r[2]*(round(extent[4L] / r[2]))

  if (isTRUE(all.equal((r[1] + x), abs(x)))) {
    x <- abs(x)
  }
  if (isTRUE(all.equal((r[2] + y), abs(y)))) {
    y <- abs(y)
  }
  return(c(x, y))
}

#' Crop an extent, snapped to the grain
#'
#' A crop (or extend), it snaps the input extent to the origin of the input
#' extent (based on the dimension) #' Note that snap is modelled on the
#' behaviour of the raster package, and is different from projwin in GDAL (WIP
#' to illustrate).
#' @param x extent
#' @inheritParams grid
#' @param snap out by default, may be near or in
#'
#' @return aligned extent
#' @export
#'
#' @examples
#' align_extent(c(4.5, 5.6, 2, 4), c(10, 5), c(0, 10, 0, 5))
align_extent <- function(x, dimension, extent = NULL, snap = c("out", "near", "in")) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension)
  snap <- match.arg(snap)
  res <- c(x_res(dimension, extent), y_res(dimension, extent))
  orig <- origin(dimension, extent)
  xmin <- x[1L]
  xmax <- x[2L]
  ymin <- x[3L]
  ymax <- x[4L]
  # snap points to pixel boundaries
  if (snap == 'near') {
    xmn <- round((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- round((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- round((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- round((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  } else if (snap == 'out') {
    xmn <- floor((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- ceiling((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- floor((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- ceiling((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  } else if (snap == 'in') {
    xmn <- ceiling((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- floor((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- ceiling((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- floor((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  }

  if (xmn == xmx) {
    if (xmn <= xmin) {
      xmx <- xmx + res[1]
    } else {
      xmn <- xmn - res[1]
    }
  }
  if (ymn == ymx) {
    if (ymn <= ymin) {
      ymx <- ymx + res[2]
    } else {
      ymn <- ymn - res[2]
    }
  }
  c(xmn, xmx, ymn, ymx)
}




## align_extent is a crop (or extend), it snaps the input extent to the origin of the input extent (based on the dimension)
#ne <- align_extent(x, c(360, 180), c(-180, 180, -90, 90))
## e_dim is the same for the dimension
#e_dim(ne, c(360, 180), c(-180, 180, -90, 90))

#' Virtual grid modification
#'
#' To modify a grid is to align an extent to the grid origin.  Modification includes to reduce or extend the area covered,
#' in either dimension.  This implies a new extent, snapped
#' to the grain of the origin grid and a new size (dimension in x,y).
#'
#' This works for any grid, the input extent can be within the original, an extension of the original, or
#' completely non-intersecting the original grid.
#'
#'
#' @param x extent of candidate grid (vector of xmin,xmax,ymin,ymax)
#' @inheritParams grid
#' @param ... ignored
#' @param snap one of "out" (default), "near", or "in"
#' @export
#' @examples
#' ## any arbitrary extent
#' x <- c(sort(runif(2, -180, 180)), sort(runif(2, -90, 90)))
#' print(x)
#' vcrop(x,  c(360, 180), c(-180, 180, -90, 90))
vcrop <- function(x,  dimension, extent = NULL, ..., snap = "out") {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension)
  new_extent <- align_extent(x, dimension, extent,  snap = snap)
  badx <- all(new_extent[1:2] <= extent[1L]) || all(new_extent[1:2] >= extent[2L])
  bady <- all(new_extent[3:4] <= extent[3L]) || all(new_extent[3:4] >= extent[4L])
  if (badx || bady) message("extents do not overlap")
#   print(new_extent)
#   print(snap)
#   print(dimension)
#   print(extent)
# print(  extent_dimension(new_extent, dimension, extent, snap = snap))
 list(extent = new_extent,
       dimension = extent_dimension(new_extent, dimension, extent, snap = snap))
}

