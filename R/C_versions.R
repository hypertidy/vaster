# All definitions here are for comparing results with R versions, and are placeholder
# for future work on perfomance for many conversions.
#
# Indices here are 0-based. Columns count from the left, rows from the top,
# matching col_from_x()/row_from_y() and x_from_col()/y_from_row(). Rows are counted
# down from ymax in C (not flipped afterwards) so that a y exactly on a row boundary
# goes to the same row as in row_from_y().

row_from_y_c <- function(dimension, extent, y) {
  .Call("index_from_coord_", as.integer(dimension[2L]), as.double(extent[3:4]), as.double(y), TRUE, PACKAGE = "vaster")
}
col_from_x_c <- function(dimension, extent, x) {
  .Call("index_from_coord_", as.integer(dimension[1L]), as.double(extent[1:2]), as.double(x), FALSE, PACKAGE = "vaster")
}

x_from_col_c <- function(dimension, extent, col) {
  .Call("coord_from_index_", as.integer(dimension[1L]), as.double(extent[1:2]), as.integer(col), PACKAGE = "vaster")
}
y_from_row_c <- function(dimension, extent, row) {
  nr <- as.integer(dimension[2L])
  .Call("coord_from_index_", nr, as.double(extent[3:4]), nr - 1L - as.integer(row), PACKAGE = "vaster")
}


x_centre_c <- function(dimension, extent) {
  .Call("coord_centre_", as.integer(dimension[1L]), as.double(extent[1:2]), PACKAGE = "vaster")
}
y_centre_c <- function(dimension, extent) {
  .Call("coord_centre_", as.integer(dimension[2L]), as.double(extent[3:4]), PACKAGE = "vaster")
}
