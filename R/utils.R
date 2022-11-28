"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
dimension0 <- function(dimension) {
  if (length(dimension) < 1 || !is.numeric(dimension) || anyNA(dimension[1:2])) stop("dimension must be valid numeric 'ncol[, nrow]'")
  rep(dimension, length.out = 2L)
}
extent0 <- function(dimension) {
  .check_args(dimension)
  c(0, dimension[1L], 0, dimension[2L])
}

.check_args <-function(dimension, extent = NULL) {
  if (is.null(dimension) || !length(dimension) == 2L) stop("dimension must be length = 2")
  if (!is.numeric(dimension)) stop("dimension must be integer/numeric")
  if (anyNA(dimension) || any(dimension < 1)) stop("dimension must be length 2, valid values for ncol,nrow >=1")
  if (!is.null(extent)) {
    if (!length(extent) == 4L) stop("extent must be length = 4 xmin,xmax,ymin,ymax")
    if (!is.numeric(extent)) stop("extent must be numeric")
    if (anyNA(extent)) stop("extent must be length 4, valid values for xmin, xmax, ymin, ymax")
    if (diff(extent[1:2]) <= 0) stop("invalid xmin,xmax - must be xmax > xmin")
    if (diff(extent[3:4]) <= 0) stop("invalid ymin,ymax - must be ymax > ymin")

  }
  invisible(NULL)
}
