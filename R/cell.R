#' Cells
#'
#' Functions that work with cells.
#'
#' The cell is indexed from the top left corner and proceeds to the right, and
#' then down scanning by rows. The n cell is a the bottom right corner.
#' Orientation is different to R's native matrix order, but see (WiP doc and
#' helpers for conversion).
#' @inheritParams grid
#' @param xy matrix of coordinates
#' @param x_extent extent to find cells of
#' @param cell cells to find extent, or row,col, or xy of
#' @param row row to find cell of
#' @param col column to find cell of
#' @name cells
NULL

#' @name cells
#' @return cell index
#' @export
#'
#' @examples
#' cell_from_xy(c(10, 5), extent = c(0, 10, 0, 5), cbind(5, 4))
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



#' @name cells
#' @return cells of extent
#' @export
#' @examples
#' cell_from_extent(c(10, 5), c(0, 10, 0, 5), c(6, 7, 2, 3))
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


#' @name cells
#' @return extent of cells
#' @export
#' @examples
#' extent_from_cell(c(10, 5), c(0, 10, 0, 5), c(4, 5))
extent_from_cell <- function(dimension, extent = NULL, cell) {
  extent <- extent %||% extent0(dimension)

  .check_args(dimension, extent)
  extent <- extent %||% extent0(dimension)

  cell <- stats::na.omit(unique(round(cell)))
  cell <- cell[cell > 0 & cell <= n_cell(dimension)]
  if (length(cell) < 1) {
    stop('no valid cells')
  }
  r <- c(x_res(dimension, extent), y_res(dimension, extent))
  dx <- r[1] * c(-0.5, 0.5)
  dy <- r[2] * c(-0.5, 0.5)
  c(range(x_from_cell(dimension, extent, cell)) + dx, range(y_from_cell(dimension, extent, cell)) + dy)
}


#' @name cells
#' @return row,col of cells
#' @export
#'
#' @examples
#' rowcol_from_cell(c(10, 5), c(0, 10, 0, 5), 3:5)
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

#' @name cells
#' @return xy from cells
#' @export
#' @examples
#' xy_from_cell(c(10, 5), c(0, 10, 0, 5), 4:6)
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

#' @name cells
#' @return x of cells
#' @export
#'
#' @examples
#' x_from_cell(c(10, 5), c(0, 10, 0, 5), 4:7)
x_from_cell <- function(dimension, extent = NULL, cell) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  ## improve with x_from_col
  x_from_col(dimension, extent, col_from_cell(dimension, cell))
}

#' @name cells
#' @return y of cells
#' @export
#'
#' @examples
#' y_from_cell(c(10, 5), c(0, 10, 0, 5), 4:7)
y_from_cell <- function(dimension, extent = NULL, cell) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  ## improve with y_from_row
  y_from_row(dimension, extent, row_from_cell(dimension, cell))
}




#' @name cells
#' @return col of cells
#' @export
#'
#' @examples
#' col_from_cell(c(10, 5),  4:7)
col_from_cell <-function(dimension, cell) {
  .check_args(dimension)

  cell <- round(cell)
  cell[cell < 1L | cell > prod(dimension)] <- NA
  rownr <- trunc((cell - 1)/dimension[1L]) + 1L
  as.integer(cell - ((rownr - 1) * dimension[1L]))
}

#' @name cells
#' @return row of cells
#' @export
#'
#' @examples
#' row_from_cell(c(10, 5),  4:7)
row_from_cell <-function(dimension, cell) {
  .check_args(dimension)
  cell <- round(cell)
  cell[cell < 1 | cell > prod(dimension)] <- NA
  trunc((cell - 1)/dimension[1L]) + 1
}

#' @name cells
#' @return cell of rows
#' @export
#'
#' @examples
#' cell_from_row(c(10, 5), 4:7)
cell_from_row <- function(dimension, row) {
  .check_args(dimension)
  row <- round(row)
  cols <- rep(seq_len(dimension[1L]), times=length(row))
  rows <- rep(row, each=dimension[1])
  cell_from_row_col(dimension, rows, cols)
}


#' @name cells
#' @return cell of cols
#' @export
#'
#' @examples
#' cell_from_col(c(10, 5), 4:7)
cell_from_col <- function(dimension, col) {
  .check_args(dimension)
  col <- round(col)
  rows <- rep(seq_len(dimension[2L]), times = length(col))
  cols <- rep(col, each = dimension[2])
  cell_from_row_col(dimension, rows, cols)
}

#' @name cells
#' @return cell of row,col
#' @export
#'
#' @examples
#' cell_from_row_col(c(10, 5), 1:4, 4:7)
cell_from_row_col <- function(dimension, row, col) {
  .check_args(dimension)
  colrow <- cbind(as.vector(col), as.vector(row))  ## for recycling
  colnr <- colrow[,1L]
  rownr <- colrow[,2L]

  nr <- dimension[2L]
  nc <- dimension[1L]
  i <- seq_along(rownr)-1
  nn <- length(rownr)

  r <- rownr[ifelse(i < nn, i, i %% nn) + 1]
  c <- colnr[ifelse(i < nn, i, i %% nn) + 1]
  ifelse(r < 1 | r > nr | c < 1 | c > nc, NA,  (r-1) * nc + c)
}

#' @name cells
#' @return cell of row,col combined
#' @export
#'
#' @examples
#' cell_from_rowcol_combine(c(10, 5), 1:4, 4:7)
cell_from_rowcol_combine <- function(dimension, row, col) {
  .check_args(dimension)
  row[row < 1 | row > n_row(dimension)] <- NA
  col[col < 1 | col > n_col(dimension)] <- NA
  cols <- rep(col, times = length(row))
  dim(cols) <- c(length(col), length(row))
  cols <- t(cols)
  row <- (row - 1) * n_col(dimension)
  cols <- cols + row
  as.vector(t(cols))
}
