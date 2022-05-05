#' Title
#'
#' @param extent
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
x_res <- function(extent, dimension) {
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
y_res <- function(extent, dimension) {
  diff(extent[3:4])/dimension[2L]
}


## e_dim is align_extent for the dimension (input is the output of align_extent)
e_dim <- function(x, extent, dimension) {
  c(diff(x[1:2]) / x_res(extent, dimension),
    diff(x[3:4]) / y_res(extent, dimension))
}


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
