row_from_y_c <- function(dimension, extent, y) {
  .Call("row_from_y_", as.integer(dimension[2L]), as.double(extent[3:4]), as.double(y), PACKAGE = "vaster")
}
col_from_x_c <- function(dimension, extent, x) {
  .Call("col_from_x_", as.integer(dimension[1L]), as.double(extent[1:2]), as.double(x), PACKAGE = "vaster")
}
