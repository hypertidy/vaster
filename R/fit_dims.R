##' Aspect ratio of dimension conflated with bbox
##' 
##' Generate an appropriate dimension (shape, ncol,nrow) from an 
##' input width(height). If heignt not specified we have a square. 
##' 
##' @param size seed dimension size
##' @param wh distance across dimension span/s
##' 
##' @return description dimension `c(ncol, nrow)` 
##' @examples
##' fit_dims(256, c(10, 20))
##' fit_dims(1024, c(102723, 1e5))
fit_dims <- function(size = 1024L, wh = c(size, size)) {
  wh <- rep(wh, length.out = 2L)
  w <- wh[1L]; h <- wh[2L];
  as.integer(ceiling(size * c(w, h) / max(w, h)))
}
