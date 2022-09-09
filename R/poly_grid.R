expgrid <- function(x, y) cbind(x, rep(y, each = length(x)))


poly_grid <- function(dimension, extent = NULL, cell= NULL) {
  dimension <- rep(dimension, length.out = 2L)
  if (is.null(extent)) extent <- c(0, dimension[1L], 0, dimension[2L])
  xc <- vaster::x_corner(extent = extent, dimension = dimension)
  yc <- rev(vaster::y_corner(extent = extent, dimension = dimension))
  xlen <- dm[1L] + 1
  ylen <- dm[2L] + 1
  cds <- cbind(
    expgrid(xc[-xlen], yc[-ylen]), # topleft
    expgrid(xc[-1L], yc[-ylen]),
    expgrid(xc[-1L],  yc[-1L]),
    expgrid(xc[-xlen], yc[-1L]),
    expgrid(xc[-xlen], yc[-ylen]))
  if (!is.null(cell)) {
    cellrange <- range(cell)
    if (anyNA(cellrange)) stop("cell has missing values")
    if (any(cellrange < 1)) stop("cell has values less than 1")
    if (any(cellrange > prod(dimension))) stop("cell has values greater than number of cells")
    cds <- cds[cell, , drop = FALSE]

  }
  d <- data.frame(x = as.vector(t(cds[, c(1, 3, 5, 7, 9)])),
                  y  = as.vector(t(cds[, c(2, 4, 6, 8, 10)])))
  if (is.null(cell)) cell <- seq_len(prod(dimension))
  d$linestring_id <- rep(seq_len(prod(dimension))[cell], each = 5L)
  d
}

