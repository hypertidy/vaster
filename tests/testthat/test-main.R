
dm <- c(10, 12)
dm_bad <- c(2, NA)
dm_neg <- c(-1, 2)

ex <- c(-5, 5, -2, 2)
ex_bad <- c(-5, NA, -2, 0)
ex_ord <- c(5, -5, -2, 2)



test_that("cell ops work", {
  expect_error(n_cell(1))
  expect_error(n_cell(dm_bad))
  expect_error(n_cell(dm_neg))
  expect_equal(n_cell(dm), 120)

  expect_error(n_col(1))
  expect_error(n_col(dm_bad))
  expect_error(n_col(dm_neg))
  expect_equal(n_col(dm), 10)

  expect_error(n_col(1))
  expect_error(n_col(dm_bad))
  expect_error(n_col(dm_neg))
  expect_equal(n_row(dm), 12)
})

test_that("coordinate ranges work", {
  expect_error(x_centre(1))
  expect_error(x_centre(dm_bad))
  expect_error(x_centre(dm_neg))

  expect_error(x_centre(dm, ex_bad))
  expect_error(x_centre(dm, ex_ord))
  expect_equal(x_centre(dm), seq(0.5, dm[1] - 0.5))
  expect_equal(x_centre(dm, ex)[2L], -3.5)
  expect_length(x_centre(dm, ex), dm[1])

  expect_error(y_centre(1))
  expect_error(y_centre(dm_bad))
  expect_error(y_centre(dm_neg))

  expect_error(y_centre(dm, ex_bad))
  expect_error(y_centre(dm, ex_ord))
  expect_equal(y_centre(dm), seq(0.5, dm[2] - 0.5))
  expect_equal(y_centre(dm, ex)[2L], -1.5)
  expect_length(y_centre(dm, ex), dm[2])



  expect_error(x_corner(1))
  expect_error(x_corner(dm_bad))
  expect_error(x_corner(dm_neg))

  expect_error(x_corner(dm, ex_bad))
  expect_error(x_corner(dm, ex_ord))
  expect_equal(x_corner(dm), seq(0, dm[1]))
  expect_equal(x_corner(dm, ex)[2L], -4)
  expect_length(x_corner(dm, ex), dm[1] + 1)

  expect_error(y_corner(1))
  expect_error(y_corner(dm_bad))
  expect_error(y_corner(dm_neg))

  expect_error(y_corner(dm, ex_bad))
  expect_error(y_corner(dm, ex_ord))
  expect_equal(y_corner(dm), seq(0, dm[2]))
  expect_length(y_corner(dm, ex), dm[2] + 1)



  expect_error(x_min(dm, ex_bad))
  expect_error(x_max(dm, ex_bad))
  expect_error(y_min(dm, ex_bad))
  expect_error(y_max(dm, ex_bad))
  expect_error(xlim(dm, ex_bad))
  expect_error(ylim(dm, ex_bad))
  expect_error(x_res(dm, ex_bad))
  expect_error(y_res(dm, ex_bad))

  expect_error(x_min(dm_bad))
  expect_error(x_max(dm_bad))
  expect_error(y_min(dm_bad))
  expect_error(y_max(dm_bad))
  expect_error(xlim(dm_bad))
  expect_error(ylim(dm_bad))
  expect_error(x_res(dm_bad))
  expect_error(y_res(dm_bad))



  expect_equal(x_min(dm), 0)
  expect_equal(x_max(dm), dm[1L])
  expect_equal(y_min(dm), 0)
  expect_equal(y_max(dm), dm[2])
  expect_equal(xlim(dm), c(0, dm[1L]))
  expect_equal(ylim(dm), c(0, dm[2L]))
  expect_equal(x_res(dm), 1)
  expect_equal(y_res(dm), 1)

  expect_equal(x_min(dm, ex), ex[1L])
  expect_equal(x_max(dm, ex), ex[2L])
  expect_equal(y_min(dm, ex), ex[3L])
  expect_equal(y_max(dm, ex), ex[4L])
  expect_equal(xlim(dm, ex), ex[1:2])
  expect_equal(ylim(dm, ex), ex[3:4])

  expect_equal(x_res(dm, ex), diff(ex[1:2])/dm[1L])
  expect_equal(y_res(dm, ex), diff(ex[3:4])/dm[2L])




})
