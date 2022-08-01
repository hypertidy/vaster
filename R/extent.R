#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
xlim <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[c(1L, 2L)]
}
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
ylim <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[c(3L, 4L)]
}
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
x_min <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[1L]
}
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples

x_max <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[2L]
}
#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples

y_min <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[3L]
}

#' Title
#'
#' @inheritParams x_res
#'
#' @return
#' @export
#'
#' @examples
y_max <- function(dimension, extent = NULL) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  extent[4L]
}




