#' Grid
#'
#' Basic grid tools, cell, resolution, dimension, extent.
#'
#' @name grid
NULL

#' @name grid
#' @return number of cells
#' @export
#' @examples
#' n_cell(c(10, 5))
n_cell <- function(dimension) {
  .check_args(dimension)
  prod(dimension)
}

#' @name grid
#' @return x resolution (width of cell)
#' @param dimension integer ncol, nrow
#' @param extent numeric extent xmin,xmax,ymin,ymax
#' @export
#' @examples
#' x_res(c(10, 5), c(0, 10, 0, 5))
x_res <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  diff(extent[1:2])/dimension[1L]
}

#' @name grid
#' @return y resolution (height of cell)
#' @export
#' @examples
#' y_res(c(10, 5), c(0, 10, 0, 5))
y_res <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  diff(extent[3:4])/dimension[2L]
}

#' @name grid
#' @return number of rows
#' @export
#' @examples
#' n_row(c(10, 5))
n_row <- function(dimension) {
  .check_args(dimension)
  dimension[2L]
}

#' @name grid
#' @return number of cols
#' @export
#' @examples
#' n_col(c(10, 5))
n_col <- function(dimension) {
  .check_args(dimension)
  dimension[1L]
}


#' @name grid
#' @return x extent (corner to corner)
#' @export
#' @examples
#' xlim(c(10, 5), c(0, 10, 0, 5))
xlim <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[c(1L, 2L)]
}


#' @name grid
#' @return y extent (corner to corner)
#' @export
#' @examples
#' ylim(c(10, 5), c(0, 10, 0, 5))
ylim <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[c(3L, 4L)]
}

#' @name grid
#' @return x minimum (left edge)
#' @export
#' @examples
#' x_min(c(10, 5), c(0, 10, 0, 5))
x_min <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[1L]
}

#' @name grid
#' @return x maximum (right edge)
#' @export
#' @examples
#' x_max(c(10, 5), c(0, 10, 0, 5))
x_max <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[2L]
}


#' @name grid
#' @return y minimum (bottom edge)
#' @export
#' @examples
#' y_min(c(10, 5), c(0, 10, 0, 5))
y_min <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[3L]
}

#' @name grid
#' @return ymaximum (top edge)
#' @export
#' @examples
#' y_max(c(10, 5), c(0, 10, 0, 5))
y_max <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[4L]
}





