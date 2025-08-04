#' Plot an extent
#'
#' @param x extent xmin,xmax,ymin,ymax
#' @param ... arguments passed to [graphics::rect()]
#' @param asp aspect ratio (1 by default)
#' @param add add to plot or initiated one, `FALSE` by default
#' @param border colour of lines of extent
#' @return nothing, used for plot side effect
#' @export
#'
#' @examples
#' plot_extent(c(-180, 180, -90, 90))
#' plot_extent(c(100, 150, -60, -30), add = TRUE, border = "firebrick")
plot_extent <- function(x, ..., asp = 1, add = FALSE,  border = "black") {
  x <- matrix(unlist(x, use.names = FALSE), ncol = 4L)

  if (!add) {
    xlab = ""; ylab = ""
    plot(range(x[,1:2], na.rm = TRUE), range(x[,3:4], na.rm = TRUE),  type = "n", xlab = xlab, ylab = ylab, asp = asp)
  }
  rect(x[,1L], x[,3L], x[,2L], x[,4L], border = border, ...)

}

#' @importFrom graphics abline
plot_grid <- function(dimension, extent = NULL, ...) {
  if (is.null(extent)) extent <- c(0, dimension[1L], 0, dimension[2L])
  plot_extent(extent, ...)
  graphics::abline(v = x_corner(dimension, extent), h = y_corner(dimension, extent))
}

#' Draw extent
#'
#' Draw an extent with two clicks
#'
#' @param show the drawn extent
#' @param ... arguments pass to [graphics::rect()]
#'
#' @returns an extent, numeric vector of xmin,xmax,ymin,ymax
#' @importFrom graphics locator rect
#' @export
#' @examples
#' if (interactive()) {
#' plot(1)
#' draw_extent(show = TRUE)  ## click twice on the plot
#' }
#'
draw_extent <- function(show = TRUE, ...) {
  x <- unlist(lapply(locator(2L), sort), use.names = FALSE)
  if (show) rect(x[1L], x[3L], x[2L], x[4L], ...)
  x
}
