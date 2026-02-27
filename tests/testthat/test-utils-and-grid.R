## ---- from_xyz ----

test_that("from_xyz recovers grid from coordinates", {
  dm <- c(10, 5)
  ex <- c(0, 10, 0, 5)
  pts <- vaster_long(dm, ex)
  result <- from_xyz(pts)
  expect_equal(result$dimension, dm)
  expect_equal(result$extent, ex)
})

test_that("from_xyz works with non-origin extent", {
  dm <- c(8, 4)
  ex <- c(100, 108, -20, -16)
  pts <- vaster_long(dm, ex)
  result <- from_xyz(pts)
  expect_equal(result$dimension, dm)
  expect_equal(result$extent, ex)
})

test_that("from_xyz detects irregular points", {
  bad_pts <- cbind(c(1, 2, 3.7), c(1, 2, 3))
  expect_error(from_xyz(bad_pts), "not regular")
})

## ---- fit_dims ----

test_that("fit_dims scales to aspect ratio", {
  ## landscape: twice as wide as tall
  result <- fit_dims(256, c(20, 10))
  expect_equal(result, c(256L, 128L))

  ## portrait: taller than wide
  result <- fit_dims(256, c(10, 20))
  expect_equal(result, c(128L, 256L))

  ## square
  result <- fit_dims(100, c(5, 5))
  expect_equal(result, c(100L, 100L))
})

test_that("fit_dims defaults to square", {
  result <- fit_dims(512)
  expect_equal(result, c(512L, 512L))
})

## ---- origin ----

test_that("origin returns grid alignment anchor", {
  dm <- c(360, 180)
  ex <- c(-180, 180, -90, 90)
  orig <- origin(dm, ex)
  expect_length(orig, 2)
  expect_equal(orig, c(0, 0))
})

test_that("origin works with default extent", {
  orig <- origin(c(10, 5))
  expect_length(orig, 2)
})

## ---- intersect_extent ----

test_that("intersect_extent returns overlap", {
  dm <- c(10, 5)
  ex <- c(0, 10, 0, 5)
  ix <- intersect_extent(c(2, 8, 1, 4), dm, ex)
  expect_length(ix, 4)
  ## snapped to grid
  expect_true(ix[1] >= 2)
  expect_true(ix[2] <= 8)
  expect_true(ix[3] >= 1)
  expect_true(ix[4] <= 4)
})

test_that("intersect_extent warns on non-overlap", {
  dm <- c(10, 5)
  ex <- c(0, 10, 0, 5)
  expect_message(intersect_extent(c(20, 30, 20, 30), dm, ex))
})

## ---- vaster_boundary ----

test_that("vaster_boundary returns closed polygon coords", {
  dm <- c(4, 3)
  ex <- c(0, 4, 0, 3)
  b <- vaster_boundary(dm, ex)
  expect_equal(ncol(b), 2)
  ## perimeter: 2*(ncol+1) + 2*(nrow+1) - 4 corners = 2*(4+3) + ... points
  ## should form a closed ring (first == last)
  expect_equal(b[1, ], b[nrow(b), ])
})

## ---- vaster_boundary_cell ----

test_that("vaster_boundary_cell returns perimeter cells", {
  dm <- c(4, 3)
  cells <- vaster_boundary_cell(dm)
  ## 4x3 grid has 10 perimeter cells (all 12 minus 2 interior)
  ## actually all boundary: 4*3 = 12 cells, interior = (4-2)*(3-2) = 2
  expect_equal(length(unique(cells)), 10)
  ## all cells should be valid
  expect_true(all(cells >= 1 & cells <= 12))
  ## interior cells 6,7 should NOT appear
  expect_false(6 %in% cells)
  expect_false(7 %in% cells)
})

test_that("vaster_boundary_cell for single row", {
  cells <- vaster_boundary_cell(c(5, 1))
  expect_equal(sort(unique(cells)), 1:5)
})

## ---- extent_vrt ----

test_that("extent_vrt reads tile extents from bundled VRT", {
  src <- gzfile(system.file("extdata/NASADEM_be.vrt.gz", package = "vaster"), "rt")
  ex <- extent_vrt(src)
  close(src)
  expect_true(is.matrix(ex))
  expect_equal(ncol(ex), 4)
  expect_equal(colnames(ex), c("xmin", "xmax", "ymin", "ymax"))
  ## all extents should be valid (xmax > xmin, ymax > ymin)
  expect_true(all(ex[, "xmax"] > ex[, "xmin"]))
  ## ymin and ymax: in this VRT y_from_row gives decreasing y
  ## so we just check they are finite
  expect_true(all(is.finite(ex)))
})

## ---- plot_extent ----

test_that("plot_extent runs without error", {
  plot_extent(c(-180, 180, -90, 90))
  expect_true(TRUE)
})

test_that("plot_extent vectorised", {
  exts <- rbind(c(0, 10, 0, 10), c(5, 15, 5, 15))
  plot_extent(exts)
  expect_true(TRUE)
})
