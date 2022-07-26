#' Convert to long form (XYZ[,Z2,etc])
#'
#' Matrix of x, y[, data] in raster order, use 'raster_order = FALSE' for traditional R matrix 'x[i], y[i]' order
#'
#' @param extent
#' @param dimension
#' @param data
#' @param raster_order
#'
#' @return
#' @export
#'
#' @examples
#' # see https://gist.github.com/mdsumner/b844766f28910a3f87dc2c8a398a3a13
vaster_long <- function(dimension, extent = NULL, data = NULL, raster_order = TRUE) {
  extent <- extent %||% extent0(dimension)

    three <- if (length(dim(data)) == 3L) 3 else NULL
    if (!is.null(data)) {
      data <- aperm(data, c(2, 1, three))
      data <- matrix(data, prod(dm))
    }
    xyz <- cbind(xy_from_cell(ex, dm, seq_len(prod(dm))), data)
  if (!raster_order) {
   xyz <- xyz[order(xyz[,2L], xyz[,1L]), ]
  }
  xyz
}

#' Image trad form
#'
#' @param extent
#' @param dimension
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' ## see https://gist.github.com/mdsumner/b844766f28910a3f87dc2c8a398a3a13
vaster_listxyz <- function(dimension, extent = NULL, data = NULL) {
  extent <- extent %||% extent0(dimension)

  if (is.null(data)) {
    data <- matrix(FALSE, dm[2], dm[1])
  }
  if (length(dim(data)) > 2) {
    message("multi array not supported, this is trad image( ) format")
    data <- data[,,1L]  ## should warn
  }
  list(x = x_from_col(extent, dimension, seq_len(dimension[1])),
           y = rev(y_from_row(extent, dimension, seq_len(dimension[2]))), z = t(data[nrow(data):1, ]))
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
x_corner <- function(x) {
  xl <- x$extent[1:2]
  ##resx <- vaster:::x_res(x$extent, x$dimension)
  seq(xl[1L], xl[2L], length.out = x$dimension[1L] + 1L)
}
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
y_corner <- function(x) {
  yl <- x$extent[3:4]
  ##resy <- vaster:::y_res(x$tileraster$extent, x$tileraster$dimension)
  seq(yl[1L], yl[2L], length.out = x$dimension[2L] + 1L)
}


#' Title
#'
#' @param extent
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
x_centre <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)

  xl <- xlim(extent)
  resx <- x_res(extent, dimension)
  seq(xl[1L] + resx/2, xl[2L] - resx/2, length.out = n_col(dimension))
}
#' Title
#'
#' @param extent
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
y_centre <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)

  yl <- ylim(extent)
  resy <- y_res(extent, dimension)
  seq(yl[1L] + resy/2, yl[2L] - resy/2, length.out = n_row(dimension))
}


#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
x_from_col <- function(dimension, extent = NULL, col) {
  extent <- extent %||% extent0(dimension)

  col[col < 1] <- NA
  col[col > dimension[1L]] <- NA
  x_centre(extent, dimension)[col]
}
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
y_from_row <- function(dimension, extent = NULL, row) {
  extent <- extent %||% extent0(dimension)

  row[row < 1] <- NA
  row[row > dimension[2]] <- NA
  rev(y_centre(extent, dimension))[row]
}

#' Title
#'
#' @param extent
#'
#' @param dimension
#' @param x
#'
#' @export
col_from_x <- function(dimension, extent = NULL, x) {
  extent <- extent %||% extent0(dimension)

  colnr <- trunc((x - x_min(extent)) / x_res(extent, dimension)) + 1
  colnr[ x == x_max(extent) ] <- n_col(dimension)
  colnr[ x < x_min(extent) | x > x_max(extent) ] <- NA
  return(as.vector(colnr))
}
#' Title
#'
#' @param extent
#'
#' @param dimension
#' @param y
#'
#' @export
row_from_y <- function(dimension, extent = NULL, y) {
  extent <- extent %||% extent0(dimension)

  rownr <- 1 + (trunc((y_max(extent) - y) / y_res(extent, dimension)))
  rownr[y == y_min(extent) ] <- n_row(dimension)
  rownr[y > y_max(extent) | y < y_min(extent)] <- NA
  return(as.vector(rownr))
}
#' Title
#'
#' @param extent
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
coords <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)

  cell <- seq_len(n_cell(dimension))
  cbind(x = x_from_cell(extent, dimension, cell),
        y = y_from_cell(extent, dimension, cell))
}
