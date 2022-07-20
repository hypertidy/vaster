
cell_kernel <- function(dimension, cell, size = 1) {
  ## offset is
  row <- row_from_cell(dimension, cell)
  col <- col_from_cell(dimension, cell)
  offset <- c(col + c(-1, 1) * size, row + c(-1, 1) * size)
  offset[c(1, 3)] <- pmax(offset[c(1, 3)], 0)  ## 0-based for vapour_read_raster
  offset[c(2, 4)] <- pmin(offset[c(2, 4)], dimension-1)

  outdim <- diff(offset)[c(1, 3)] + 1
  c(offset[c(1, 3)], outdim)
}
cell_kernel(c(100, 50), 3)

## cell
kernel_group_cell <- function(dimension, cell, size = 1) {
  len <- (size + 2)^2
  row <- row_from_cell(dimension, cell)
  col <- col_from_cell(dimension, cell)
  idx <- seq(-size, size)
  rows <- matrix(rep(row, each = len), len) + matrix(rep(idx, each = size + 2), len, length(row))
  cols <- matrix(rep(col, each = len), len) + matrix(rep(idx, each = size + 2), len, length(col))
  cell_from_row_col(dimension, rows, cols)
}







library(raadtools)
library(vaster)
r <- terra::rast(topofile("etopo2"))
#writeRaster(r, "inst/misc/etopo2.tif", gdal = list("COMPRESS=DEFLATE"), filetype = "COG")
#system("gdalinfo inst/misc/etopo2.tif")
#Band 1 Block=512x512 Type=Float32, ColorInterp=Gray
#Overviews: 5400x2700, 2700x1350, 1350x675, 675x337, 337x168

f <- "inst/misc/etopo2.tif"
dimension <- vapour::vapour_raster_info(f)$dimension
cell <- sample(1:prod(dimension), 1)
ck <- cell_kernel(dimension, cell)
library(furrr)
plan(multisession)
system.time({
out <- future_map_int(sample(1:prod(dimension), dimension[1]),
           function(.x) max(vapour::vapour_read_raster_int(f, window = cell_kernel(dimension, .x))))

})



## what scheme do we want to run kernel ops
## 1. read a big chunk of raster aligned to the kernel
## 2. index that chunk so it's expanded out as if every cell indexed its kernel
## 3. filter where the kernel weighting is zero
## 4. run the op
## 5. write the chunk

size <- 1
kernel <- matrix(1, size * 2 + 1, size * 2 + 1)
offset <- c(0, 0)
chunk <- 1024
dim <- chunk + (chunk %% (size * 2 + 1)) + 1

library(vapour)
data <- vapour_read_raster_int(f, window = c(offset, rep(dim, 2)))


