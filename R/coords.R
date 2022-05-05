x_corner <- function(x) {
  xl <- x$extent[1:2]
  ##resx <- vaster:::x_res(x$extent, x$dimension)
  seq(xl[1L], xl[2L], length.out = x$dimension[1L] + 1L)
}
y_corner <- function(x) {
  yl <- x$extent[3:4]
  ##resy <- vaster:::y_res(x$tileraster$extent, x$tileraster$dimension)
  seq(yl[1L], yl[2L], length.out = x$dimension[2L] + 1L)
}
