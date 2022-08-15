#' Draw extent
#'
#' Draw an extent with two clicks
draw_extent <- function(show = TRUE, ...) {
  x <- unlist(lapply(locator(2L), sort), use.names = FALSE)
  if (show) rect(x[1L], x[3L], x[2L], x[4L], ...)
  x
}

# draw <- function(n = Inf, close = FALSE, ...) {
#   for (i)
# }
#' Convert to long form '(XYZ[,Z2,etc])'
#'
#' Matrix of 'x, y[, data]' in raster order, use 'raster_order = FALSE' for traditional R matrix 'x[i], y[i]' order
#'
#' @inheritParams x_res
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
  colnames(xyz) <- c("x", "y", "z")
    xyz
}

#' Image trad form
#'
#' @inheritParams x_res
#' @param data
#'
#' @return
#' @export
#'
#' @examples
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
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
x_corner <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  xl <- extent[1:2]
  ##resx <- vaster:::x_res(x$extent, x$dimension)
  seq(xl[1L], xl[2L], length.out = dimension[1L] + 1L)
}
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
y_corner <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  yl <- extent[3:4]
  ##resy <- vaster:::y_res(x$tileraster$extent, x$tileraster$dimension)
  seq(yl[1L], yl[2L], length.out = dimension[2L] + 1L)
}


#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
x_centre <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  xl <- xlim(dimension, extent)
  resx <- x_res(dimension, extent = extent)
  seq(xl[1L] + resx/2, xl[2L] - resx/2, length.out = n_col(dimension))
}
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
y_centre <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  yl <- ylim(dimension, extent)
  resy <- y_res(dimension, extent = extent)
  seq(yl[1L] + resy/2, yl[2L] - resy/2, length.out = n_row(dimension))
}


#' Title
#'
#' @inheritParams x_res
#' @param col
#'
#' @return
#' @export
#'
#' @examples
x_from_col <- function(dimension, extent = NULL, col) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  col[col < 1] <- NA
  col[col > dimension[1L]] <- NA
  x_centre(dimension, extent = extent)[col]
}
#' Title
#'
#' @inheritParams x_res
#' @param row
#' @return
#' @export
#'
#' @examples
y_from_row <- function(dimension, extent = NULL, row) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  row[row < 1] <- NA
  row[row > dimension[2]] <- NA
  rev(y_centre(dimension, extent = extent))[row]
}

#' Title
#'
#' @inheritParams x_res
#' @param x
#'
#' @export
col_from_x <- function(dimension, extent = NULL, x) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  colnr <- trunc((x - x_min(dimension, extent)) / x_res(dimension, extent = extent)) + 1
  colnr[ x == x_max(dimension, extent) ] <- n_col(dimension)
  colnr[ x < x_min(dimension, extent) | x > x_max(dimension, extent) ] <- NA
  return(as.vector(colnr))
}
#' Title
#'
#' @inheritParams x_res
#' @param y
#'
#' @export
row_from_y <- function(dimension, extent = NULL, y) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  rownr <- 1 + (trunc((y_max(dimension, extent) - y) / y_res(dimension, extent = extent)))
  rownr[y == y_min(dimension, extent) ] <- n_row(dimension)
  rownr[y > y_max(dimension, extent) | y < y_min(dimension, extent)] <- NA
  return(as.vector(rownr))
}
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
coords <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  cell <- seq_len(n_cell(dimension))
  cbind(x = x_from_cell(dimension, extent = extent, cell),
        y = y_from_cell(dimension, extent = extent, cell))
}
