#' Title
#'
#' @param extent
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
x_res <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  diff(extent[1:2])/dimension[1L]
}
#' Title
#'
#' @param extent
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
y_res <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  diff(extent[3:4])/dimension[2L]
}

#' Title
#'
#' @param dimension
#'
#' @export
n_row <- function(dimension) {
  dimension[2L]
}

#' Title
#'
#' @param dimension
#'
#' @export
n_col <- function(dimension) {
  dimension[1L]
}


#' Title
#'
#' @param dimension
#'
#' @export
n_cell <- function(dimension) {
  prod(dimension)
}
