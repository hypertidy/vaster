#' Plot an extent
#'
#' @param x extent xmin,xmax,ymin,ymax
#' @param ... arguments passed to [graphics::rect()]
#' @param asp aspect ratio (1 by default)
#' @param add add to plot or initiated one, `FALSE` by default
#' @param border colour of lines of extent
#'
#' @return nothing, used for plot side effect
#' @export
#'
#' @examples
#' plot_extent(c(-180, 180, -90, 90))
#' plot_extent(c(100, 150, -60, -30), add = TRUE, border = "firebrick")
plot_extent <- function(x, ..., asp = 1, add = FALSE,  border = "black") {
  if (!add) {
    xlab = ""; ylab = ""
    plot(x[1:2], x[3:4],  type = "n", xlab = xlab, ylab = ylab, asp = asp)
  }
  rect(x[1L], x[3L], x[2L], x[4L], border = border, ...)

}


#' Draw extent
#'
#' Draw an extent with two clicks
#'
#' @param plot the drawn extent
#' @param ... arguments pass to [graphics::rect()]
#'
#' @importFrom graphics locator rect
draw_extent <- function(show = TRUE, ...) {
  x <- unlist(lapply(locator(2L), sort), use.names = FALSE)
  if (show) rect(x[1L], x[3L], x[2L], x[4L], ...)
  x
}
