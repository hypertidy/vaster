## ---- geotransform / world file creators ----

test_that("geo_transform0 creates correct vector", {
  gt <- geo_transform0(px = c(1, -1), ul = c(0, 10))
  expect_length(gt, 6)
  expect_named(gt, c("xmin", "xres", "yskew", "ymax", "xskew", "yres"))
  expect_equal(gt[["xmin"]], 0)
  expect_equal(gt[["xres"]], 1)
  expect_equal(gt[["ymax"]], 10)
  expect_equal(gt[["yres"]], -1)
  expect_equal(gt[["xskew"]], 0)
  expect_equal(gt[["yskew"]], 0)
})

test_that("geo_world0 creates correct vector", {
  wf <- geo_world0(px = c(1, -1), ul = c(0, 10))
  expect_length(wf, 6)
  expect_named(wf, c("xres", "yskew", "xskew", "yres", "xmin", "ymax"))
  ## world file uses centre-of-pixel offset
  expect_equal(wf[["xmin"]], 0.5)
  expect_equal(wf[["ymax"]], 10 + (-1)/2)
})

test_that("geotransform and world round-trip", {
  gt <- geo_transform0(px = c(0.5, -0.25), ul = c(100, 50))
  wf <- geotransform_to_world(gt)
  gt2 <- world_to_geotransform(wf)
  expect_equal(gt2, gt, tolerance = 1e-10)
})

test_that("geotransform and world round-trip with shear", {
  gt <- geo_transform0(px = c(1, -1), ul = c(0, 0), sh = c(0.1, 0.2))
  wf <- geotransform_to_world(gt)
  gt2 <- world_to_geotransform(wf)
  expect_equal(gt2, gt, tolerance = 1e-10)
})

## ---- extent <-> geotransform ----

test_that("extent_dim_to_gt and gt_dim_to_extent round-trip", {
  ex <- c(0, 10, -5, 5)
  dm <- c(20, 10)
  gt <- extent_dim_to_gt(ex, dm)
  ex2 <- gt_dim_to_extent(gt, dm)
  expect_equal(ex2, ex, tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("gt_dim_to_extent matches known values", {
  ## 10x5 grid from (0,5) to (10,0) => extent c(0, 10, 0, 5)
  gt <- geo_transform0(px = c(1, -1), ul = c(0, 5))
  ex <- gt_dim_to_extent(gt, c(10, 5))
  expect_equal(ex, c(0, 10, 0, 5), ignore_attr = TRUE)
})

## ---- RasterIO ----

test_that("rasterio0 creates named vector", {
  rio <- rasterio0(c(0L, 0L), src_dim = c(24L, 10L))
  expect_length(rio, 6)
  expect_named(rio, c("offset_x", "offset_y", "source_nx", "source_ny",
                       "out_nx", "out_ny"))
  expect_equal(unname(rio[1:2]), c(0, 0))
  expect_equal(unname(rio[3:4]), c(24, 10))
  ## out_dim defaults to src_dim
  expect_equal(unname(rio[5:6]), c(24, 10))
})

test_that("rasterio0 with different out_dim", {
  rio <- rasterio0(c(0L, 0L), src_dim = c(100L, 50L), out_dim = c(10L, 5L))
  expect_equal(unname(rio[5:6]), c(10, 5))
})

test_that("rasterio_idx creates from dimension", {
  rio <- rasterio_idx(c(100, 50))
  expect_equal(unname(rio[1:2]), c(0, 0))
  expect_equal(unname(rio[3:4]), c(100, 50))
})

test_that("rasterio_to_sfio and sfio_to_rasterio round-trip", {
  rio <- rasterio0(c(5L, 10L), src_dim = c(24L, 10L), out_dim = c(12L, 5L))
  sfio <- rasterio_to_sfio(rio)
  expect_true(is.list(sfio))
  expect_true("nXOff" %in% names(sfio))
  ## sf uses 1-based offsets
  expect_equal(sfio$nXOff, 6, ignore_attr = TRUE)
  expect_equal(sfio$nYOff, 11, ignore_attr = TRUE)

  rio2 <- sfio_to_rasterio(sfio)
  expect_equal(unname(rio2[1:6]), unname(rio[1:6]), ignore_attr = TRUE)
})

test_that("raster_sfio creates sf-format RasterIO", {
  sfio <- raster_sfio(c(100, 50))
  expect_true(is.list(sfio))
  expect_equal(sfio$nXSize, 100, ignore_attr = TRUE)
  expect_equal(sfio$nYSize, 50, ignore_attr = TRUE)
})

test_that("raster_sfio with downsampling factor", {
  sfio <- raster_sfio(c(100, 50), fact = 2)
  expect_equal(sfio$nBufXSize, 50, ignore_attr = TRUE)
  expect_equal(sfio$nBufYSize, 25, ignore_attr = TRUE)
})
