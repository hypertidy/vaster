#' Title
#' @inheritParams x_res
#' @return
#' @export
n_cell <- function(dimension) {
  .check_args(dimension)
  prod(dimension)
}

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





