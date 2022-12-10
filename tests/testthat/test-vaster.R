
fives <- structure(c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5,
                     0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 0.5, 1.5, 2.5,
                     3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5,
                     6.5, 7.5, 8.5, 9.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5,
                     9.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 3.5, 3.5,
                     3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 2.5, 2.5, 2.5, 2.5, 2.5,
                     2.5, 2.5, 2.5, 2.5, 2.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5,
                     1.5, 1.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), dim = c(50L,
                                                                                          2L), dimnames = list(NULL, c("x", "y")))


ms <- list(x = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5),
           y = c(0.5, 1.5, 2.5, 3.5, 4.5), z = structure(c(FALSE, FALSE,
                                                           FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                                           FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                                           FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                                           FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                                           FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                                           FALSE, FALSE, FALSE), dim = c(10L, 5L)))
test_that("coordinate generations works", {
  expect_equal(vaster_long(c(10, 5), c(0, 10, 0, 5)), fives)
  expect_equal(vaster_listxyz(c(10, 5), c(0, 10, 0, 5)), ms)
})


test_that("utils works", {
   expect_equal(dimension0(c(10, 5)), c(10, 5))

  expect_equal(extent0(c(10, 5)), c(0, 10, 0, 5))
}
          )
