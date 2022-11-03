# cell_from_col <- function(dimension, col) {
#   cell_from_row <- function(dimension, row) {
#     cell_from_row_col <- function(dimension, row, col) {
#       cell_from_row_col_combine <- function(dimension, row, col) {
#         col_from_cell <-function(dimension, cell) {
#           row_from_cell <-function(dimension, cell) {
#
#             dimension+extent
#             cell_from_xy <- function(dimension, extent, xy) {
#               col_from_x <- function(extent, dimension, x) {
#                 coords <- function(extent, dimension) {
#                   extent_from_cell <- function(extent, dimension, cells) {
#
#                     row_from_y <- function(extent, dimension, y) {
#                       rowcol_from_cell <- function(extent, dimension, cell) {
#
#                         x_from_cell <- function(extent, dimension, cell) {
#                           x_from_col <- function(extent, dimension, col) {
#                             xy_from_cell <- function(extent, dimension, cell) {
#
#                               y_from_cell <- function(extent, dimension, cell) {
#                                 y_from_row <- function(extent, dimension, row) {



dm <- c(360, 180)
dm_bad <- c(NA,5)
dm_neg <- c(6, -2)

ex <- c(-180, 180, -90, 90)
ex_bad <- c(-5, 5, NA, 0)
ex_ord <- c(5, 15, 20, 2)

xy <- cbind(c(0, 100, -100, -30, 25),
            c(0, 80, -80, -20, -25))
test_that("bad inputs fail", {
  expect_error(cell_from_col(dm_bad, 10))
  expect_error(cell_from_row(dm_bad, 10))
  expect_error(cell_from_row_col(dm_bad, 10))
  expect_error(cell_from_rowcol_combine(dm_bad, 10))
  expect_error(col_from_cell(dm_bad, 10))
  expect_error(row_from_cell(dm_bad, 10))

  expect_error(cell_from_col(dm_neg, 10))
  expect_error(cell_from_row(dm_nge, 10))
  expect_error(cell_from_row_col(dm_nge, 10))
  expect_error(cell_from_rowcol_combine(dm_neg, 10))
  expect_error(col_from_cell(dm_neg, 10))
  expect_error(row_from_cell(dm_neg, 10))

  expect_error(cell_from_xy(dm, ex_bad, xy))
  expect_error(col_from_x(dm, ex_bad, xy[,1]))
  expect_error(xy(dm, ex_bad))
  expect_error(extent_from_cell(dm, ex_bad, 10:1))
  expect_error(row_from_y(dm, ex_bad, xy[,2]))
  expect_error(rowcol_from_cell(dm, ex_bad, 20:30))
  expect_error(x_from_cell(dm, ex_bad, 5:6))
  expect_error(x_from_col(dm, ex_bad, 60:80))
  expect_error(xy_from_cell(dm, ex_bad, 100:200))
  expect_error(y_from_cell(dm, ex_bad, 5:15))
  expect_error(y_from_row(dm, ex_bad, 20:1))

  expect_error(cell_from_xy(dm, ex_ord))
  expect_error(col_from_x(dm, ex_ord))
  expect_error(xy(dm, ex_ord))
  expect_error(extent_from_cell(dm, ex_ord))
  expect_error(row_from_y(dm, ex_ord))
  expect_error(rowcol_from_cell(dm, ex_ord))
  expect_error(x_from_cell(dm, ex_ord))
  expect_error(x_from_col(dm, ex_ord))
  expect_error(xy_from_cell(dm, ex_ord))
  expect_error(y_from_cell(dm, ex_ord))
  expect_error(y_from_row(dm, ex_ord))

  expect_error(cell_from_extent(dm, ex_bad, c(0, 10, 1, 3)))
  expect_error(cell_from_extent(dm_bad, c(0, 10, 1, 3)))
  expect_error(extent_from_cell(dm, ex, c(NA, NA, NA)))

})

test_that("cell funcs work", {
  expect_equal(cell_from_col(dm, 1), seq(1, 64441, by = 360))

  expect_equal(cell_from_col(dm, 10), seq(10, 64450, by = 360))
  expect_equal(cell_from_row(dm, 10), 3241:3600)
  expect_equal(cell_from_row_col(dm, 10, 20), 3260)
  expect_equal(cell_from_rowcol_combine(dm, 10:11, 20:21), c(3260, 3261, 3620, 3621))
  expect_equal(col_from_cell(dm, 1000), 280)
  expect_equal(row_from_cell(dm, 1000), 3)

  expect_equal(cell_from_xy(dm, ex, xy), c(32581, 3881, 61281, 39751, 41606))
  expect_equal(col_from_x(dm, ex, xy[,1]), c(181, 281, 81, 151, 206))
  expect_equal(dim(xy(dm, ex)), c(64800L, 2))
  expect_equal(extent_from_cell(dm, ex, 10:1), c(-180, -170,   89,   90))

  expect_equal(row_from_y(dm, ex, xy[,2]), c(91, 11, 171, 111, 116))
  expect_equal(rowcol_from_cell(dm, ex, 20:30), structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 20L,
                                                            21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L), dim = c(11L,
                                                                                                                       2L), dimnames = list(NULL, c("row", "col"))))
  expect_equal(x_from_cell(dm, ex, 5:6), c(-175.5, -174.5))
  expect_equal(x_from_col(dm, ex, 60:80), c(-120.5, -119.5, -118.5, -117.5, -116.5, -115.5, -114.5, -113.5,
                                            -112.5, -111.5, -110.5, -109.5, -108.5, -107.5, -106.5, -105.5,
                                            -104.5, -103.5, -102.5, -101.5, -100.5))
  expect_equal(dim(xxyy <- xy_from_cell(dm, ex, 100:200)), c(101, 2L))
  expect_equal(range(xxyy), c(-80.5,  89.5))

  expect_equal(y_from_cell(dm, ex, 5:15), rep(89.5, 11))
  expect_equal(y_from_row(dm, ex, 20:1),c(70.5, 71.5, 72.5, 73.5, 74.5, 75.5, 76.5, 77.5, 78.5, 79.5,
                                          80.5, 81.5, 82.5, 83.5, 84.5, 85.5, 86.5, 87.5, 88.5, 89.5))

  expect_equal(cell_from_extent(dm, ex, c(142, 143, -42, -30)), c(43523, 43883, 44243, 44603, 44963, 45323, 45683, 46043, 46403,
                                                                      46763, 47123, 47483))

}
)

