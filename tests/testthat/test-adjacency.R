## 4x3 grid layout (4 cols, 3 rows):
##  1  2  3  4
##  5  6  7  8
##  9 10 11 12

test_that("adjacency queen returns correct neighbours", {
  dm <- c(4, 3)
  nb <- adjacency(dm, cell = 6)
  expect_equal(ncol(nb), 8)
  expect_equal(colnames(nb), c("up", "down", "left", "right",
                               "upleft", "upright", "downleft", "downright"))
  ## cell 6 is row 2, col 2: neighbours are known

  expect_equal(nb[1, "up"], 2, ignore_attr = TRUE)
  expect_equal(nb[1, "down"], 10, ignore_attr = TRUE)
  expect_equal(nb[1, "left"], 5, ignore_attr = TRUE)
  expect_equal(nb[1, "right"], 7, ignore_attr = TRUE)
  expect_equal(nb[1, "upleft"], 1, ignore_attr = TRUE)
  expect_equal(nb[1, "upright"], 3, ignore_attr = TRUE)
  expect_equal(nb[1, "downleft"], 9, ignore_attr = TRUE)
  expect_equal(nb[1, "downright"], 11, ignore_attr = TRUE)
})

test_that("adjacency rook returns 4 columns", {
  nb <- adjacency(c(4, 3), cell = 6, directions = "rook")
  expect_equal(ncol(nb), 4)
  expect_equal(colnames(nb), c("up", "down", "left", "right"))
})

test_that("adjacency bishop returns 4 columns", {
  nb <- adjacency(c(4, 3), cell = 6, directions = "bishop")
  expect_equal(ncol(nb), 4)
  expect_equal(colnames(nb), c("upleft", "upright", "downleft", "downright"))
})

test_that("adjacency gives NA at edges", {
  dm <- c(4, 3)
  ## cell 1 is top-left corner
  nb <- adjacency(dm, cell = 1)
  expect_true(is.na(nb[1, "up"]))
  expect_true(is.na(nb[1, "left"]))
  expect_true(is.na(nb[1, "upleft"]))
  expect_true(is.na(nb[1, "upright"]))
  expect_true(is.na(nb[1, "downleft"]))
  expect_equal(nb[1, "down"], 5, ignore_attr = TRUE)
  expect_equal(nb[1, "right"], 2, ignore_attr = TRUE)
  expect_equal(nb[1, "downright"], 6, ignore_attr = TRUE)

  ## cell 12 is bottom-right corner
  nb12 <- adjacency(dm, cell = 12)
  expect_true(is.na(nb12[1, "down"]))
  expect_true(is.na(nb12[1, "right"]))
  expect_true(is.na(nb12[1, "downright"]))
  expect_true(is.na(nb12[1, "downleft"]))
  expect_true(is.na(nb12[1, "upright"]))
  expect_equal(nb12[1, "up"], 8, ignore_attr = TRUE)
  expect_equal(nb12[1, "left"], 11, ignore_attr = TRUE)
  expect_equal(nb12[1, "upleft"], 7, ignore_attr = TRUE)
})

test_that("adjacency vectorised over cells", {
  nb <- adjacency(c(4, 3), cell = 1:12)
  expect_equal(nrow(nb), 12)
})

test_that("adjacency rejects bad dimension", {
  expect_error(adjacency(c(NA, 3), cell = 1))
  expect_error(adjacency(c(-1, 3), cell = 1))
})
