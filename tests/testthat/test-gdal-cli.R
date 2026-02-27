test_that("gdal_ts formats dimension", {
  expect_equal(gdal_ts(c(100, 50)), "-ts 100 50")
})

test_that("gdal_te formats extent with GDAL order", {
  ## vaster order: xmin, xmax, ymin, ymax
  ## GDAL -te order: xmin, ymin, xmax, ymax
  expect_equal(gdal_te(c(0, 10, -5, 5)), "-te 0 -5 10 5")
})

test_that("ts_te combines both", {
  result <- ts_te(c(100, 50), c(0, 10, -5, 5))
  expect_true(grepl("-ts 100 50", result))
  expect_true(grepl("-te 0 -5 10 5", result))
})
