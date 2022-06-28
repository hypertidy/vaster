#' Title
#'
#' @param x_extent
#' @param extent
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
intersect_extent <- function(x_extent, extent, dimension) {
  vcrop(x_extent, extent, dimension)$extent
}



## e_dim is align_extent for the dimension (input is the output of align_extent)
e_dim <- function(x, extent, dimension) {
  c(diff(x[1:2]) / x_res(extent, dimension),
    diff(x[3:4]) / y_res(extent, dimension))
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
origin <-   function(extent, dimension) {
  r <- c(x_res(extent, dimension), y_res(extent, dimension))
  x <- extent[1L] - r[1]*(round(extent[1L] / r[1]))
  y <- extent[4L] - r[2]*(round(extent[4L] / r[2]))

  if (isTRUE(all.equal((r[1] + x), abs(x)))) {
    x <- abs(x)
  }
  if (isTRUE(all.equal((r[2] + y), abs(y)))) {
    y <- abs(y)
  }
  return(c(x, y))
}

## align_extent is a crop (or extend), it snaps the input extent to the origin of the input extent (based on the dimension)
#' Title
#'
#' @param x
#' @param extent
#' @param dimension
#' @param snap
#'
#' @return
#' @export
#'
#' @examples
align_extent <- function(x, extent, dimension, snap = c("out", "near", "in")) {
  snap <- match.arg(snap)
  res <- c(x_res(extent, dimension), y_res(extent, dimension))
  orig <- origin(extent, dimension)
  xmin <- x[1L]
  xmax <- x[2L]
  ymin <- x[3L]
  ymax <- x[4L]
  # snap points to pixel boundaries
  if (snap == 'near') {
    xmn <- round((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- round((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- round((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- round((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  } else if (snap == 'out') {
    xmn <- floor((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- ceiling((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- floor((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- ceiling((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  } else if (snap == 'in') {
    xmn <- ceiling((xmin-orig[1]) / res[1]) * res[1] + orig[1]
    xmx <- floor((xmax-orig[1]) / res[1]) * res[1] + orig[1]
    ymn <- ceiling((ymin-orig[2]) / res[2]) * res[2] + orig[2]
    ymx <- floor((ymax-orig[2]) / res[2]) * res[2] + orig[2]
  }

  if (xmn == xmx) {
    if (xmn <= xmin) {
      xmx <- xmx + res[1]
    } else {
      xmn <- xmn - res[1]
    }
  }
  if (ymn == ymx) {
    if (ymn <= ymin) {
      ymx <- ymx + res[2]
    } else {
      ymn <- ymn - res[2]
    }
  }
  c(xmn, xmx, ymn, ymx)
}




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
#' @export
#' @examples
#' ## any arbitrary extent
#' x <- c(sort(runif(2, -180, 180)), sort(runif(2, -90, 90)))
#' print(x)
#' vcrop(x,  c(-180, 180, -90, 90), c(360, 180))
vcrop <- function(x,  extent, dimension, ..., snap = "out") {
  new_extent <- align_extent(x, extent, dimension,  snap = snap)
  list(extent = new_extent,
       dimension = e_dim(new_extent, extent, dimension))
}

