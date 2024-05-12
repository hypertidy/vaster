row_from_y_c <- function(dimension, extent, y) {
  .Call("index_from_coord_", as.integer(dimension[2L]), as.double(extent[3:4]), as.double(y), PACKAGE = "vaster")
}
col_from_x_c <- function(dimension, extent, x) {
  .Call("index_from_coord_", as.integer(dimension[1L]), as.double(extent[1:2]), as.double(x), PACKAGE = "vaster")
}

x_from_col_c <- function(dimension, extent, col) {
  .Call("coord_from_index_", as.integer(dimension[1L]), as.double(extent[1:2]), as.integer(col))
}
y_from_row_c <- function(dimension, extent, row) {
  .Call("coord_from_index_", as.integer(dimension[2L]), as.double(extent[3:4]), as.integer(row))
}


x_centre_c <- function(dimension, extent) {
  .Call("coord_centre_", as.integer(dimension[1L]), as.double(extent[1:2]))
}
y_centre_c <- function(dimension, extent) {
  .Call("coord_centre_", as.integer(dimension[2L]), as.double(extent[3:4]))
}
