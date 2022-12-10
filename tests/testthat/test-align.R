ex <- c(-81.8762817382812, -50.9523010253906, 66.7496643066406, 78.3925399780273)

glex <- c(-180, 180, -90, 90)
dm <- c(4320, 2160)
# library(terra)
# terra <- list()
# for (a in c("out", "in", "near")) {
#   r <- crop(rast(ext(glex), ncols = dm[1], nrows = dm[2]),
#        ext(ex), snap = a)
# terra[[a]] <- list(dimension = dim(r)[2:1], extent = c(xmin(r), xmax(r), ymin(r), ymax(r)))
# }
# dput(terra)
terra <- list(out = list(dimension = c(372, 141), extent = c(-81.9166666666667,
                                                    -50.9166666666667, 66.6666666666667, 78.4166666666667)), `in` = list(
                                                      dimension = c(370, 139), extent = c(-81.8333333333333, -51,
                                                                                          66.75, 78.3333333333333)), near = list(dimension = c(372,
                                                                                                                                               140), extent = c(-81.9166666666667, -50.9166666666667, 66.75,
                                                                                                                                                                78.4166666666667)))
library(vaster)
for (a in c("out", "in", "near")) {
  expect_equal(align_extent(ex, dimension = dm, extent = glex, snap = a),
                    terra[[a]]$extent, ignore_attr = TRUE)
  expect_equal(extent_dimension(ex, dimension = dm, extent = glex, snap = a),
                    terra[[a]]$dimension)


  v <- vcrop(ex, dimension = dm, extent = glex, snap = a)
  expect_equal(v$extent, terra[[a]]$extent, ignore_attr = TRUE)
  #expect_equal(v$dimension, terra[[a]]$dimension)
}
## should get this
#ex0 <- c(-81.9166666666667, -50.9166666666667, 66.6666666666667, 78.4166666666667)
align_extent(ex, dimension = c(4320, 2160), c(-180, 180, -90, 90))
extent_dimension(ex, dimension = c(4320, 2160), c(-180, 180, -90, 90))

## vcrop is WRONG
vcrop(ex, dimension = c(4320, 2160), c(-180, 180, -90, 90))
vaster::x_res(c(4320, 2160), c(-180, 180, -90, 90))




align_extent(ex, dimension = c(4320, 2160), c(-180, 180, -90, 90), snap = "in")
extent_dimension(ex, dimension = c(4320, 2160), c(-180, 180, -90, 90), snap = "in")

test_that("snap works", {
  expect_equal(snap_extent(c(0, 4.2, 2.1, 3), 1), c(0, 5, 2, 4))
  expect_equal(snap_extent(c(0, 4.2, 2.1, 3), 5), c(0, 5, 0, 5))
  expect_equal(buffer_extent(c(0, 4.2, 2.1, 3), 1), c(0, 5, 2, 4))
  expect_equal(buffer_extent(c(0, 4.2, 2.1, 3), 5), c(0, 5, 0, 5))

})
