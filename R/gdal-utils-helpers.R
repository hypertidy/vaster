#' @export
#' @name ts_te
te <- function(extent) {
  sprintf("-te %s", paste0(extent[c(1, 3, 2, 4)], collapse = " "))
}
#' @export
#' @name ts_te
ts <- function(dimension) {
  sprintf("-ts %s", paste0(dimension[1:2], collapse = " "))
}


#' Target size, extent
#'
#' Format properties for the GDAL options.
#'
#' @param dimension ncol, nrow
#' @param extent xmin,xmax,ymin,ymax
#'
#' @return string formatted for GDAL command line (-te -ts)
#'
#' @export
#' @name ts_te
#'
#' @examples
#' ts_te(c(10, 100), 1:4)
#' ts(c(10, 100))
#' te(1:4)
ts_te <- function(dimension, extent) {
  paste0(ts(dimension), te(extent), collapse = " ")
}
