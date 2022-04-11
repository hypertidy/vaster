



## align_extent is a crop (or extend), it snaps the input extent to the origin of the input extent (based on the dimension)
#ne <- align_extent(x, c(360, 180), c(-180, 180, -90, 90))
## e_dim is the same for the dimension
#e_dim(ne, c(360, 180), c(-180, 180, -90, 90))

#' Virtual grid modification
#'
#' To modify a grid is to align an extent to the grid origin.  Modification includes to reduce or extend the area covered,
#' in either dimension.  This implies a new extent, snapped
#' to the grain of the origin grid and a new size (dimension in x,y).
#'
#' This works for any grid, the input extent can be within the original, an extension of the original, or
#' completely non-intersecting the original grid.
#'
#'
#' @param x extent of candidate grid (vector of xmin,xmax,ymin,ymax)
#' @param extent extent of original grid (vector of xmin,xmax,ymin,ymax)
#' @param dimension size of original grid vector of x-cells, y-cellss (number of cells in each direction)
#' @param ... ignored
#' @param snap one of "out" (default), "near", or "in"
#' @examples
#' ## any arbitrary extent
#' x <- c(sort(runif(2, -180, 180)), sort(runif(2, -90, 90)))
#' print(x)
#' vcrop(x, c(360, 180), c(-180, 180, -90, 90))
vcrop <- function(x,  extent, dimension, ..., snap = "out") {
  new_extent <- align_extent(x, extent, dimension,  snap = snap)
  list(extent = new_extent,
       dimension = e_dim(new_extent, extent, dimension))
}

