## tiles
## we need row,col,cell converters for mosaics


## these are internal use versions that don't detect NA out of bounds
col_from_x_inf <- function(dimension, extent = NULL, x) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  colnr <- trunc((x - x_min(dimension, extent)) / x_res(dimension, extent = extent)) + 1
  colnr[ x == x_max(dimension, extent) ] <- n_col(dimension)
  #colnr[ x < x_min(dimension, extent) | x > x_max(dimension, extent) ] <- NA
  return(as.vector(colnr))
}

row_from_y_inf <- function(dimension, extent = NULL, y) {
  extent <- extent %||% extent0(dimension)
  .check_args(dimension, extent)

  rownr <- 1 + (trunc((y_max(dimension, extent) - y) / y_res(dimension, extent = extent)))
  rownr[y == y_min(dimension, extent) ] <- n_row(dimension)
#  rownr[y > y_max(dimension, extent) | y < y_min(dimension, extent)] <- NA
  return(as.vector(rownr))
}


## child from parent or parent from child (using _inf versions that allow out of bounds row/col numbers)
cell2cell <- function(dimension, extent, ## that's the parent
                      dim0, ext0, cell0 ) {    ## the chid tile) {

                        ## checks
                      orig <- origin(dimension, extent)
                      orig0 <- origin(dim0, ext0)
                      stopifnot(all.equal(orig, orig0))

                      res <- c(x_res(dimension, extent), y_res(dimension, extent))
                      res0 <- c(x_res(dim0, ext0), y_res(dim0, ext0))
                        stopifnot(all.equal(res, res0))

                      ## the col,row of the parent where the child starts (tr)
                      cr <- c(col_from_x_inf(dimension, extent, ext0[1]),
                              row_from_y_inf(dimension, extent, ext0[4]))



                     # print(cr)
                      rc0 <- rowcol_from_cell(dim0, ext0, cell0)

                      cr_global <- cbind(cr[1] + rc0[,2] - 1,
                                     cr[2] + rc0[,1] - 1)
                      cell_from_row_col(dimension,  cr_global[,2], cr_global[,1])
}


