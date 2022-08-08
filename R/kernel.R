# f <- "inst/misc/etopo2.tif"
# dimension <- vapour::vapour_raster_info(f)$dimension
# cell <- sample(1:prod(dimension), 1)
# ck <- cell_kernel(dimension, cell)
# library(furrr)
# plan(multisession)
# system.time({
#   out <- future_map_int(sample(1:prod(dimension), dimension[1]),
#                         function(.x) max(vapour::vapour_read_raster_int(f, window = cell_kernel(dimension, .x))))
#
# })
#
#

#' convert cell to windows c(offset, dim) - 0-based for vapour_read_raster
#'
#' @inheritParams x_res
#' @param cell cell to convert (not vectorize)
#' @param size size of kernel (1 is 3x3)
#'
#' @return
#' @noRd
#'
#' @examples
#' cell_kernel(c(100, 50), 3)
cell_kernel <- function(dimension, cell, size = 1) {
  .check_args(dimension)
  ## offset is
  row <- row_from_cell(dimension, cell)
  col <- col_from_cell(dimension, cell)
  offset <- c(col + c(-1, 1) * size, row + c(-1, 1) * size)
  offset[c(1, 3)] <- pmax(offset[c(1, 3)], 0)  ## 0-based for vapour_read_raster
  offset[c(2, 4)] <- pmin(offset[c(2, 4)], dimension-1)

  outdim <- diff(offset)[c(1, 3)] + 1
  c(offset[c(1, 3)], outdim)
}

#' Vectorized kernel to cell index
#'
#' @inheritParams x_res
#' @param cell cells to obtain kernel index of
#' @param size size of kernel (1 is 3x3)
#'
#' @return
#' @noRd
#'
#' @examples
#' f <- "inst/misc/etopo2.tif"
#' # what scheme do we want to run kernel ops
#' # 1. read a big chunk of raster aligned to the kernel
#' # 2. index that chunk so it's expanded out as if every cell indexed its kernel
#' # 3. filter where the kernel weighting is zero, cell out of bounds
#' # 4. run the op
#' # 5. write the chunk
#
#' size <- 1
#' kernel <- matrix(1, size * 2 + 1, size * 2 + 1)
#' offset <- c(0, 0)
#' chunk <- 1024
#' dimension <- rep(chunk + (chunk %% (size * 2 + 1)) + 1, 2)
#'
#' library(vapour)
#' data <- vapour_read_raster_int(f, window = c(offset, dimension))
#' idx <- kernel_group_cell(dimension, seq_len(prod(dimension)), size = size)
#' cidx <- rep(seq_len(prod(dimension)), each = (size + 2)^2)
#' bad <- is.na(idx)
#' index <- tibble::tibble(data = as.vector(data)[idx[!bad]], cell = cidx[!bad])
#' library(dplyr)
#' d <- index |> group_by(cell) |> summarize(data = max(data))
#' library(ximage)
#' ximage(matrix(d$data, dimension[2], byrow = TRUE))

kernel_group_cell <- function(dimension, cell, size = 1) {
  .check_args(dimension)
  len <- (size + 2)^2
  row <- row_from_cell(dimension, cell)
  col <- col_from_cell(dimension, cell)
  idx <- seq(-size, size)
  rows <- matrix(rep(row, each = len), len) + matrix(rep(idx, each = size + 2), len, length(row))
  cols <- matrix(rep(col, each = len), len) + matrix(rep(idx, each = size + 2), len, length(col))
  cell_from_row_col(dimension, rows, cols)
}







#f <- "inst/misc/etopo2.tif"
#dimension <- vapour::vapour_raster_info(f)$dimension
#cell <- sample(1:prod(dimension), 1)
#ck <- cell_kernel(dimension, cell)
#library(furrr)
#plan(multisession)
#system.time({
#out <- future_map_int(sample(1:prod(dimension), dimension[1]),
#           function(.x) max(vapour::vapour_read_raster_int(f, window = cell_kernel(dimension, .x))))

#})


