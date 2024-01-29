

#' Convert to long form coordinates
#'
#' Matrix of xyz values in raster order.
#'
#' Use 'raster_order = FALSE' for traditional R matrix x,y order
#'
#' @inheritParams grid
#' @param data data values
#' @param raster_order use raster order or native R matrix order
#'
#' @return matrix of coordinates x,y
#' @export
#'
#' @examples
#' vaster_long(c(10, 5), c(0, 10, 0, 5))
#' # see https://gist.github.com/mdsumner/b844766f28910a3f87dc2c8a398a3a13
vaster_long <- function(dimension, extent = NULL, data = NULL, raster_order = TRUE) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

    three <- if (length(dim(data)) == 3L) 3 else NULL
    if (!is.null(data)) {
      data <- aperm(data, c(2, 1, three))
      data <- matrix(data, n_cell(dimension))
    }
    xyz <- cbind(xy_from_cell(dimension, extent = extent, seq_len(n_cell(dimension))), data)
  if (!raster_order) {
   xyz <- xyz[order(xyz[,2L], xyz[,1L]), ]
  }
  colnames(xyz) <- if (is.null(data)) c("x", "y") else  c("x", "y", "z")
    xyz
}

#' Image xyz list
#'
#' Generate list of x and y rectilinear coordinates with z matrix.
#'
#' The rectilinear coordinates are degenerate (just a product of extent/dimension).
#' @inheritParams grid
#' @param data data values (length of the product of 'dimension')
#'
#' @return list with elementx x,y,z as per [graphics::image]
#' @export
#'
#' @examples
#' vaster_listxyz(c(10, 5), c(0, 10, 0, 5))
#' ## see https://gist.github.com/mdsumner/b844766f28910a3f87dc2c8a398a3a13
vaster_listxyz <- function(dimension, extent = NULL, data = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  if (is.null(data)) {
    data <- matrix(FALSE, dimension[2], dimension[1])
  }
  if (length(dim(data)) > 2) {
    message("multi array not supported, this is trad image( ) format")
    data <- data[,,1L]  ## should warn
  }
  list(x = x_from_col(dimension, extent = extent, seq_len(dimension[1])),
           y = rev(y_from_row(dimension, extent = extent, seq_len(dimension[2]))), z = t(data[nrow(data):1, ]))
}
#' Coordinates
#'
#' Functions that work with coordinates.
#'
#' @inheritParams grid
#' @param col column index
#' @param row row index
#' @param x x coordinate
#' @param y y coordinate
#' @name coordinates
NULL

#' @name coordinates
#' @return x coordinate of corners
#' @export
#' @examples
#' x_corner(c(10, 5), c(0, 10, 0, 5))
x_corner <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  xl <- extent[1:2]
  ##resx <- vaster:::x_res(x$extent, x$dimension)
  seq(xl[1L], xl[2L], length.out = dimension[1L] + 1L)
}

#' @name coordinates
#' @return y coordinate of corners
#' @export
#' @examples
#' y_corner(c(10, 5), c(0, 10, 0, 5))
y_corner <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  yl <- extent[3:4]
  ##resy <- vaster:::y_res(x$tileraster$extent, x$tileraster$dimension)
  seq(yl[1L], yl[2L], length.out = dimension[2L] + 1L)
}


#' @name coordinates
#' @return x coordinate of centres
#' @export
#' @examples
#' x_centre(c(10, 5), c(0, 10, 0, 5))
x_centre <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  xl <- xlim(dimension, extent)
  resx <- x_res(dimension, extent = extent)
  seq(xl[1L] + resx/2, xl[2L] - resx/2, length.out = n_col(dimension))
}

#' @name coordinates
#' @return y coordinate of centres
#' @export
#' @examples
#' y_centre(c(10, 5), c(0, 10, 0, 5))
y_centre <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  yl <- ylim(dimension, extent)
  resy <- y_res(dimension, extent = extent)
  seq(yl[1L] + resy/2, yl[2L] - resy/2, length.out = n_row(dimension))
}


#' @name coordinates
#' @return x coordinate of col (centre)
#' @export
#' @examples
#' x_from_col(c(10, 5), c(0, 10, 0, 5), 2:3)
x_from_col <- function(dimension, extent = NULL, col) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  col[col < 1] <- NA
  col[col > dimension[1L]] <- NA
  x_centre(dimension, extent = extent)[col]
}

#' @name coordinates
#' @return y coordinate of row (centre)
#' @export
#' @examples
#' y_from_row(c(10, 5), c(0, 10, 0, 5), 2:3)
y_from_row <- function(dimension, extent = NULL, row) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  row[row < 1] <- NA
  row[row > dimension[2]] <- NA
  rev(y_centre(dimension, extent = extent))[row]
}

#' @name coordinates
#' @return col of x coordinate
#' @export
#' @examples
#' col_from_x(c(10, 5), c(0, 10, 0, 5), 3.5 + 1:2)
col_from_x <- function(dimension, extent = NULL, x) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  colnr <- trunc((x - x_min(dimension, extent)) / x_res(dimension, extent = extent)) + 1
  colnr[ x == x_max(dimension, extent) ] <- n_col(dimension)
  colnr[ x < x_min(dimension, extent) | x > x_max(dimension, extent) ] <- NA
  return(as.vector(colnr))
}

#' @name coordinates
#' @return y coordinate (centre) of row
#' @export
#' @examples
#' row_from_y(c(10, 5), c(0, 10, 0, 5), 2:3)
row_from_y <- function(dimension, extent = NULL, y) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  rownr <- 1 + (trunc((y_max(dimension, extent) - y) / y_res(dimension, extent = extent)))
  rownr[y == y_min(dimension, extent) ] <- n_row(dimension)
  rownr[y > y_max(dimension, extent) | y < y_min(dimension, extent)] <- NA
  return(as.vector(rownr))
}

#' @name coordinates
#' @return xy coordinate (centre) of grid
#' @export
#' @examples
#' xy(c(10, 5), c(0, 10, 0, 5))
xy <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  cell <- seq_len(n_cell(dimension))
  cbind(x = x_from_cell(dimension, extent = extent, cell),
        y = y_from_cell(dimension, extent = extent, cell))
}


#' Grid boundary in native resolution
#'
#' currently only return centre coords
#' @inheritParams grid
#' @export
#' @examples
#' vaster_boundary(c(3, 4))
vaster_boundary <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  cell <- c(seq_len(dimension[1L]),
            seq(dimension[1L], by = dimension[1], length.out = dimension[2L]),
            seq(n_cell(dimension), by = -1, length.out = dimension[1L]),
            seq(n_cell(dimension) - dimension[1L] + 1, by = -dimension[1], length.out = dimension[2L]))
  xy_from_cell(dimension, extent, cell)
}
