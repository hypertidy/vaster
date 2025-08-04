#' Adjacency, for use in creating area based meshes
#'
#' Functions 'bottom left', 'top left', 'bottom right', and 'top right' named by
#' their initials, provide very low level relative positional structures for use in
#' raster logic. These are used to traverse the divide left by area-based rasters which are
#' inherently a discrete value across a finite element. If we want that element as part of a
#' continuous surface we need to find local relative values for its corners. Used in
#' quadmesh and anglr packages, and useful for calculating neighbourhood values.
#'
#' bl, tl, br, and tr originally lived in hypertidy/affinity on github.
#'
#' @param x a matrix
#' @export
#' @name adjacencies
#' @return matrix, padded by one row and one column relative to input
#' @examples
#' (m <- matrix(1:12, 3))
#' tl(m)
#' tr(m)
#' bl(m)
#' br(m)
#' tl(br(m))
#' image0(tl(br(m)))
#' text0(tl(br(m)))
#'
#' ## this gives neighbours in adjacent positions
#' m <- matrix(1:12, ncol = 3, byrow = TRUE)
#'
#' matrix(c(t(la(m)), t(ta(m)), t(ra(m)), t(ba(m))), ncol = 4)
#'
#' ## this gives neighbours in all 8 adjacent and diagonal positions
#' image(matrix(rowMeans(matrix(c(t(la(m)), t(ta(m)), t(ra(m)),
#'  t(ba(m)), t(bl(m)), t(tl(m)), t(br(m)), t(tr(m))),  ncol = 8), na.rm = TRUE),
#'  4, byrow = TRUE))
bl <- function(x) {
  cbind(NA_integer_, rbind(NA_integer_, x))[-(dim(x)[1] + 1), - (dim(x)[2] + 1)]
}
#' @export
#' @name adjacencies
tl <-  function(x) {

  cbind(NA_integer_, rbind(x, NA_integer_))[-1, -(dim(x)[2] + 1)]
}
#' @export
#' @name adjacencies
br <- function(x) {

  cbind(rbind(NA_integer_, x), NA_integer_)[-(dim(x)[1L] + 1),-1]
}
#' @export
#' @name adjacencies
tr <- function(x) {

  cbind(rbind(x, NA_integer_), NA_integer_)[-1, -1]
}

#' @export
#' @name adjacencies
la <- function(x) (cbind(NA, x[,-dim(x)[2L]]))  ## lhm() left middle
#' @export
#' @name adjacencies
ta <- function(x) (rbind(NA, x[-dim(x)[1L], ])) ## tm()  top middle
#' @export
#' @name adjacencies
ra <- function(x) (cbind(x[,-1L], NA))      ## rhm() right middle
#' @export
#' @name adjacencies
ba <- function(x) (rbind(x[-1L,], NA) )    ## bm()  bottom middle




#' @export
#' @name adjacencies
#' @param x matrix
#' @param ... arguments passed to image()
image0 <- function(x, ...) image(seq(0, nrow(x)), seq(0, ncol(x)), x, ...)
#' @export
#' @name adjacencies
#' @importFrom graphics image text
image1 <- function(x, ...) image(seq(1, nrow(x) + 1), seq(1, ncol(x) + 1), x, ...)
#' @export
#' @name adjacencies
text0 <- function(x, ...) {
  text(expand.grid(seq(0.5, by = 1, length.out = nrow(x)),
                   seq(0.5, by = 1, length.out = nrow(x))), lab = x,
       ...)
}
