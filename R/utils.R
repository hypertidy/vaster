"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
dimension0 <- function(dimension) {
  rep(dimension, length.out = 2L)
}
extent0 <- function(dimension) {
  c(0, dimension[1L], 0, dimension[2L])
}
