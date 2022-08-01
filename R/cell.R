#' Title
#'
#' @inheritParams x_res
#' @param xy
#'
#' @return
#' @export
#'
#' @examples
cell_from_xy <- function(dimension, extent = NULL, xy) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  xx <- xy[,1L, drop = TRUE]
  yy <- xy[,2L, drop = TRUE]


  len <- length(xx)
  ncols <- n_col(dimension)
  nrows <- n_row(dimension)
  xmin <- x_min(dimension, extent)
  xmax <- x_max(dimension, extent)
  ymin <- y_min(dimension, extent)
  ymax <- y_max(dimension, extent)
  yres_inv = nrows / (ymax - ymin)
  xres_inv = ncols / (xmax - xmin)
  ## cannot use trunc here because trunc(-0.1) == 0
  row = floor((ymax - yy) * yres_inv);
  ## points in between rows go to the row below
  ## except for the last row, when they must go up
  row <- ifelse(yy == ymin, nrows - 1, row)

  col = floor((xx - xmin) * xres_inv)
  ## as for rows above. Go right, except for last column
  col <- ifelse (xx == xmax, ncols-1, col)

  ifelse (row < 0 | row >= nrows | col < 0 | col >= ncols, NA_real_, row * ncols + col + 1)
}



#' Title
#'
#' @inheritParams x_res
#' @param x_extent
#'
#' @export
cell_from_extent <- function(dimension, extent = NULL, x_extent) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  .check_args(dimension, x_extent)
  x_extent <- align_extent(x_extent, dimension, extent)
  inner_ext <- intersect_extent(x_extent, dimension, extent)
  if (is.null(inner_ext)) {
    return(NULL)
  }

  srow <- row_from_y(dimension, extent = extent, inner_ext[4L] - 0.5 * y_res(dimension, extent = extent))
  erow <- row_from_y(dimension, extent = extent,   inner_ext[3L] + 0.5 * y_res(dimension, extent = extent))
  scol <- col_from_x(dimension, extent = extent,   inner_ext[1L] + 0.5 * x_res(dimension, extent = extent))
  ecol <- col_from_x(dimension, extent = extent,   inner_ext[2L] - 0.5 * x_res(dimension, extent = extent))


 cell_from_rowcol_combine(dimension, seq(srow, erow, by = 1L),
                                  seq(scol, ecol, by = 1L))
}


#' Title
#'
#' @inheritParams x_res
#' @param cells
#'
#' @export
extent_from_cell <- function(dimension, extent = NULL, cells) {
  extent <- extent %||% extent0(dimension)

  .check_args(dimension, extent)
  extent <- extent %||% extent0(dimension)

  cells <- stats::na.omit(unique(round(cells)))
  cells <- cells[cells > 0 & cells <= n_cell(dimension)]
  if (length(cells) < 1) {
    stop('no valid cells')
  }
  r <- c(x_res(dimension, extent), y_res(dimension, extent))
  dx <- r[1] * c(-0.5, 0.5)
  dy <- r[2] * c(-0.5, 0.5)
  c(range(x_from_cell(dimension, extent, cells)) + dx, range(y_from_cell(dimension, extent, cells)) + dy)
}


#' Title
#'
#' @inheritParams x_res
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
rowcol_from_cell <- function(dimension, extent = NULL, cell) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  cell <- round(cell)
  ncols <- n_col(dimension)
  cell[cell < 1 | cell > n_cell(dimension)] <- NA
  row <- as.integer(trunc((cell-1)/ncols) + 1)
  col <- as.integer(cell - ((row-1) * ncols))
  return(cbind(row, col))
}

#' Title
#'
#' @inheritParams x_res
#' @param cell
#'
#' @export
xy_from_cell <- function(dimension, extent = NULL, cell) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  xmin <- x_min(dimension, extent)
  xmax <- x_max(dimension, extent)
  ymin <- y_min(dimension, extent)
  ymax <- y_max(dimension, extent)
  len <- length(cell)
  nrows <- n_row(dimension)
  ncols <- n_col(dimension)
  yres = (ymax - ymin) / nrows
  xres = (xmax - xmin) / ncols

  c = cell - 1
  row = floor(c / ncols)
  col = c - row * ncols
  cbind((col + 0.5) * xres + xmin,
        ymax - (row + 0.5) * yres)

}

#' Title
#'
#' @inheritParams x_res
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
x_from_cell <- function(dimension, extent = NULL, cell) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  ## improve with x_from_col
  x_from_col(dimension, extent, col_from_cell(dimension, cell))
}

#' Title
#'
#' @inheritParams x_res
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
y_from_cell <- function(dimension, extent = NULL, cell) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  ## improve with y_from_row
  y_from_row(dimension, extent, row_from_cell(dimension, cell))
}
