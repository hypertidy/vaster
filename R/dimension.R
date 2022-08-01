

#' Title
#'
#' @inheritParams x_res
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
col_from_cell <-function(dimension, cell) {
  .check_args(dimension)

  cell <- round(cell)
  cell[cell < 1L | cell > prod(dimension)] <- NA
  rownr <- trunc((cell - 1)/dimension[1L]) + 1L
  as.integer(cell - ((rownr - 1) * dimension[1L]))
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
row_from_cell <-function(dimension, cell) {
  .check_args(dimension)
  cell <- round(cell)
  cell[cell < 1 | cell > prod(dimension)] <- NA
  trunc((cell - 1)/dimension[1L]) + 1
}
#' Title
#'
#' @inheritParams x_res
#' @param row
#'
#' @return
#' @export
#'
#' @examples
cell_from_row <- function(dimension, row) {
  .check_args(dimension)
  row <- round(row)
  cols <- rep(seq_len(dimension[1L]), times=length(row))
  rows <- rep(row, each=dimension[1])
  cell_from_row_col(dimension, rows, cols)
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
cell_from_col <- function(dimension, col) {
  .check_args(dimension)
  col <- round(col)
  rows <- rep(seq_len(dimension[2L]), times = length(col))
  cols <- rep(col, each = dimension[2])
  cell_from_row_col(dimension, rows, cols)
}
#' Title
#'
#' @inheritParams x_res
#' @param row
#' @param col
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @inheritParams x_res
#' @param row
#' @param col
#'
#' @return
#' @export
#'
#' @examples
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
