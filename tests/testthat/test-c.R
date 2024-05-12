dm <- c(360, 180)
dm_bad <- c(NA,5)
dm_neg <- c(6, -2)

ex <- c(-180, 180, -90, 90)
ex_bad <- c(-5, 5, NA, 0)
ex_ord <- c(5, 15, 20, 2)

xy <- cbind(c(0, 100, -100, -30, 25),
            c(0, 80, -80, -20, -25))

expect_equal(row_from_y_c(dm, ex, xy[,2]), row_from_y(dm, ex, xy[,2]) -1)
expect_error(row_from_y_c(dm, ex_bad, xy[,2]))
expect_error(row_from_y_c(dm, ex_ord, 1))

