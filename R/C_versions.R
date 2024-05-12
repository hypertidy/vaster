row_from_y_c <- function(dimension, extent, y) {
  .Call("row_from_y_C", dimension, extent, y, PACKAGE = "vaster")
}
