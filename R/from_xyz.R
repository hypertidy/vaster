
#' Derive a grid from XYZ points
#'
#' This function is very liberal, it simply finds unique x values and unique y values,
#' sorts them and finds the minimum difference between the points, then checks that rounded
#' ratio of differences to this minimum is 1.
#'
#' The points can be the full grid set, a partial set, or a superset of the grid. The resolution will simply be
#' the smallest actual difference found. (Zero is not possible because we `sort(unique(...))`).
#'
#' The z-column if present is ignored.
#'
#' @param xyz set of points xy or xyz (matrix or data frame)
#' @param digits argument passed to [round()]
#'
#' @return list with elements 'dimension', 'extent'
#' @export
#'
#' @examples
#' from_xyz(vaster_long(c(10, 5), c(0, 10, 0, 5)))
from_xyz <- function(xyz, digits = 5) {
  ## this is what raster::rasterFromXYZ does
  x <- sort(unique(xyz[,1L, drop = TRUE]))
  dx <- diff(x)
  rx <- min(dx)

  test <- sum(round(dx / rx, digits=digits) %% 1)

  if (test > 0) stop("x values are not regular")
  y <- sort(unique(xyz[,2L, drop = TRUE]))
  dy <- diff(y)
  ry <- min(dy)
  test <- sum(round(dy / ry, digits=digits) %% 1)

  if (test > 0) stop("y values are not regular")

  list(dimension = c(length(x), length(y)),
       extent = c(range(x) + c(-0.5, 0.5) * rx, range(y) + c(-0.5, 0.5) * ry))
}
