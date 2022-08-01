#' Title
#'
#' @param dimension integer ncol, nrow
#' @param extent numeric extent xmin,xmax,ymin,ymax
#'
#' @return
#' @export
#'
#' @examples
x_res <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  diff(extent[1:2])/dimension[1L]
}
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
y_res <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)
  diff(extent[3:4])/dimension[2L]
}

#' Title
#'
#' @inheritParams x_res
#'
#' @export
n_row <- function(dimension) {
  .check_args(dimension)
  dimension[2L]
}

#' Title
#'
#' @inheritParams x_res
#'
#' @export
n_col <- function(dimension) {
  .check_args(dimension)
  dimension[1L]
}


#' Title
#'
#' @inheritParams x_res
#'
#' @export
n_cell <- function(dimension) {
  .check_args(dimension)
  prod(dimension)
}
