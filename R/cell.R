#' Title
#'
#' @param dimension
#' @param extent
#' @param xy
#'
#' @return
#' @export
#'
#' @examples
cell_from_xy <- function(dimension, extent, xy) {
  xx <- xy[,1L, drop = TRUE]
  yy <- xy[,2L, drop = TRUE]


  len <- length(xx)
  ncols <- n_col(dimension)
  nrows <- n_row(dimension)
  xmin <- x_min(extent)
  xmax <- x_max(extent)
  ymin <- y_min(extent)
  ymax <- y_max(extent)
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

  ifelse (row < 0 || row >= nrows || col < 0 || col >= ncols, NA_real_, row * ncols + col + 1)
}

#' Title
#'
#' @param dimension
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
col_from_cell <-function(dimension, cell) {
    cell <- round(cell)
    cell[cell < 1L | cell > prod(dimension)] <- NA
    rownr <- trunc((cell - 1)/dimension[1L]) + 1L
    as.integer(cell - ((rownr - 1) * dimension[1L]))
  }
#' Title
#'
#' @param dimension
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
row_from_cell <-function(dimension, cell) {
    cell <- round(cell)
    cell[cell < 1 | cell > prod(dimension)] <- NA
    trunc((cell - 1)/dimension[1L]) + 1
  }
#' Title
#'
#' @param dimension
#' @param row
#'
#' @return
#' @export
#'
#' @examples
cell_from_row <- function(dimension, row) {
  row <- round(row)
  cols <- rep(seq_len(dimension[1L]), times=length(row))
  rows <- rep(row, each=dimension[1])
  cell_from_row_col(dimension, rows, cols)
}


#' Title
#'
#' @param dimension
#' @param col
#'
#' @return
#' @export
#'
#' @examples
cell_from_col <- function(dimension, col) {
  col <- round(col)
  rows <- rep(seq_len(dimension[2L]), times = length(col))
  cols <- rep(col, each = dimension[2])
  cell_from_row_col(dimension, rows, cols)
}
#' Title
#'
#' @param dimension
#' @param row
#' @param col
#'
#' @return
#' @export
#'
#' @examples
cell_from_row_col <- function(dimension, row, col) {
    colrow <- cbind(col, row)  ## for recycling
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

#' Title
#'
#' @param dimension
#' @param row
#' @param col
#'
#' @return
#' @export
#'
#' @examples
cell_from_row_col_combine <- function(dimension, row, col) {
    row[row < 1 | row > n_row(dimension)] <- NA
        col[col < 1 | col > n_col(dimension)] <- NA
        cols <- rep(col, times = length(row))
        dim(cols) <- c(length(col), length(row))
        cols <- t(cols)
        row <- (row - 1) * n_col(dimension)
        cols <- cols + row
        as.vector(t(cols))
}




#' Title
#'
#' @param dimension
#'
#' @param extent
#' @param x_extent
#'
#' @export
cell_from_extent <- function(dimension, extent, x_extent) {
  x_extent <- align_extent(x_extent, extent, dimension)
  inner_ext <- intersect_extent(x_extent, extent, dimension)
  if (is.null(inner_ext)) {
    return(NULL)
  }

  srow <- row_from_y(object, inner_ext[4L] - 0.5 * y_res(object))
  erow <- row_from_y(object,   inner_ext[3L] + 0.5 * y_res(object))
  scol <- col_from_x(object,   inner_ext[1L] + 0.5 * x_res(object))
  ecol <- col_from_x(object,   inner_ext[2L] - 0.5 * x_res(object))

  # if (expand) {
  #   srow <- srow - round((extent@ymax - innerBox@ymax) / yres(object))
  #   erow <- erow + round((innerBox@ymin - extent@ymin) / yres(object))
  #   scol <- scol - round((innerBox@xmin - extent@xmin) / xres(object))
  #   ecol <- ecol + round((extent@xmax - innerBox@xmax) / xres(object))
  # }
  #
  return(cell_from_rowcol_combine(object, srow:erow, scol:ecol))
}


#' Title
#'
#' @param extent
#'
#' @param dimension
#' @param cells
#'
#' @export
extent_from_cell <- function(extent, dimension, cells) {
  cells <- stats::na.omit(unique(round(cells)))
  cells <- cells[cells > 0 & cells <= n_cell(dimension)]
  if (length(cells) < 1) {
    stop('no valid cells')
  }
  r <- c(x_res(extent, dimension), y_res(extent, dimension))
  dx <- r[1] * c(-0.5, 0.5)
  dy <- r[2] * c(-0.5, 0.5)
  c(range(x_from_cell(extent, dimension, cells)) + dx, range(y_from_cell(extent, dimension, cells)) + dy)
}


#' Title
#'
#' @param extent
#' @param dimension
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
rowcol_from_cell <- function(extent, dimension, cell) {

  cell <- round(cell)
  ncols <- n_col(dimension)
  cell[cell < 1 | cell > n_cell(dimension)] <- NA
  row <- as.integer(trunc((cell-1)/ncols) + 1)
  col <- as.integer(cell - ((row-1) * ncols))
  return(cbind(row, col))
}

#' Title
#'
#' @param extent
#'
#' @param dimension
#' @param cell
#'
#' @export
xy_from_cell <- function(extent, dimension, cell) {
  xmin <- x_min(extent)
  xmax <- x_max(extent)
  ymin <- y_min(extent)
  ymax <- y_max(extent)
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
#' @param extent
#' @param dimension
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
x_from_cell <- function(extent, dimension, cell) {
  ## improve with x_from_col
  x_from_col(extent, dimension, col_from_cell(dimension, cell))
}

#' Title
#'
#' @param extent
#' @param dimension
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
y_from_cell <- function(extent, dimension, cell) {
  ## improve with y_from_row
  y_from_row(extent, dimension, row_from_cell(dimension, cell))
}
