
#' Title
#'
#' @param dimension
#' @param cell
#'
#' @return
#' @export
#'
#' @examples
col_from_cell <-
  function(dimension, cell) {
    cell <- round(cell)
    cell[cell < 1L | cell > prod(dimension)] <- NA
    rownr <- trunc((cell - 1)/dimension[2L]) + 1L
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
row_from_cell <-
  function(dimension, cell) {
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
  rows <- rep(row, each=x$dimension[1])
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
cell_from_row_col <-
  function(dimension, row, col) {
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
