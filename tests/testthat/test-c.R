dm <- c(360, 180)
dm_bad <- c(NA,5)
dm_neg <- c(6, -2)

ex <- c(-180, 180, -90, 90)
ex_bad <- c(-5, 5, NA, 0)
ex_ord <- c(5, 15, 20, 2)

xy <- cbind(c(0, 100, -100, -30, 25),
            c(0, 80, -80, -20, -25))

test_that("C index helpers match the R versions (0-based)", {
  expect_equal(row_from_y_c(dm, ex, xy[,2]), row_from_y(dm, ex, xy[,2]) - 1)
  expect_equal(col_from_x_c(dm, ex, xy[,1]), col_from_x(dm, ex, xy[,1]) - 1)
  ## boundaries: min and max edges go to the first and last cell, outside is NA
  expect_equal(col_from_x_c(dm, ex, c(-180, 180, 181)), c(0, 359, NA))
  expect_equal(row_from_y_c(dm, ex, c(90, -90, -91)), c(0, 179, NA))
  expect_equal(row_from_y_c(dm, ex, c(90, -90, -91)), row_from_y(dm, ex, c(90, -90, -91)) - 1)
  expect_error(row_from_y_c(dm, ex_bad, xy[,2]))
  expect_error(row_from_y_c(dm, ex_ord, 1))
})

test_that("C coordinate helpers match the R versions (0-based)", {
  expect_equal(x_from_col_c(dm, ex, 0:359), x_from_col(dm, ex, 1:360))
  expect_equal(y_from_row_c(dm, ex, 0:179), y_from_row(dm, ex, 1:180))
  expect_equal(x_from_col_c(dm, ex, c(-1L, 360L)), c(NA_real_, NA_real_))
  expect_equal(y_from_row_c(dm, ex, c(-1L, 180L)), c(NA_real_, NA_real_))
  expect_equal(x_centre_c(dm, ex), x_centre(dm, ex))
  expect_equal(y_centre_c(dm, ex), y_centre(dm, ex))
})
