g_dim <- c(500L, 300L)
g_ext <- c(140, 160, -50, -30)

## ---- is_aligned ----

test_that("grid is aligned with itself", {
  expect_true(is_aligned(g_dim, g_ext, g_dim, g_ext))
})

test_that("vcrop produces an aligned grid", {
  h <- vcrop(c(145, 155, -45, -35), g_dim, g_ext)
  expect_true(is_aligned(g_dim, g_ext, h$dimension, h$extent))
  expect_true(is_aligned(h$dimension, h$extent, g_dim, g_ext))
})

test_that("coarser grid on the same lattice is aligned, in both directions", {
  expect_true(is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(2L, 2L), c(0, 10, 0, 10)))
  expect_true(is_aligned(c(2L, 2L), c(0, 10, 0, 10), c(10L, 10L), c(0, 10, 0, 10)))
  ## coarse grid whose origin sits on the fine lattice but not on its own
  expect_true(is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(2L, 2L), c(1, 5, 1, 5)))
})

test_that("misaligned grids are detected", {
  ## origin shifted by a fraction of a cell
  expect_false(is_aligned(g_dim, g_ext, g_dim, g_ext + c(0.01, 0.01, 0, 0)))
  ## non-commensurate resolution
  expect_false(is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(3L, 3L), c(0, 10, 0, 10)))
  ## coarse grid origin off the fine lattice
  expect_false(is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(0.5, 10.5, 0.5, 10.5)))
})

test_that("alignment is not containment", {
  expect_true(is_aligned(c(10L, 10L), c(0, 10, 0, 10), c(10L, 10L), c(100, 110, 100, 110)))
})

test_that("alignment survives floating point lattices", {
  d <- c(3600L, 1800L); e <- c(-180, 180, -90, 90)
  h <- vcrop(c(-81.87, -50.95, 66.74, 78.39), d, e)
  expect_true(is_aligned(d, e, h$dimension, h$extent))
  expect_identical(grid_offset(d, e, h$dimension, h$extent),
                   c(col = 981L, row = 116L))
})

## ---- grid_offset ----

test_that("grid_offset returns 0-based named integer vector", {
  h <- vcrop(c(142, 158, -48, -32), g_dim, g_ext)
  off <- grid_offset(g_dim, g_ext, h$dimension, h$extent)
  expect_type(off, "integer")
  expect_named(off, c("col", "row"))
  expect_identical(off, c(col = 50L, row = 30L))
})

test_that("grid_offset is zero for identical grids", {
  expect_identical(grid_offset(g_dim, g_ext, g_dim, g_ext), c(col = 0L, row = 0L))
})

test_that("grid_offset may be negative or beyond the parent", {
  expect_identical(grid_offset(c(10L, 10L), c(0, 10, 0, 10), c(10L, 10L), c(-3, 7, 2, 12)),
                   c(col = -3L, row = -2L))
})

test_that("grid_offset errors on mismatched resolution or misalignment", {
  expect_error(grid_offset(g_dim, g_ext, c(250L, 150L), g_ext), "identical resolution")
  expect_error(grid_offset(c(10L, 10L), c(0, 10, 0, 10), c(10L, 10L), c(0.5, 10.5, 0, 10)),
               "not aligned")
})

test_that("grid_offset of every tile matches its recorded offset", {
  tl <- grid_tiles(g_dim, g_ext, c(128L, 100L))
  for (t in tl) {
    expect_identical(grid_offset(g_dim, g_ext, t$dimension, t$extent), t$offset)
  }
})

## ---- snap_grid ----

test_that("snap_grid aligns a finer grid to a parent lattice", {
  p_dim <- c(10L, 10L); p_ext <- c(0, 10, 0, 10)
  s <- snap_grid(c(10L, 10L), c(2.3, 7.3, 2.3, 7.3), p_dim, p_ext)
  expect_identical(s$dimension, c(10L, 10L))
  expect_equal(s$extent, c(2, 7, 2, 7))
  expect_true(is_aligned(p_dim, p_ext, s$dimension, s$extent))
})

test_that("snap_grid keeps a coarser grid's resolution and still aligns", {
  ## this case was broken in align: snapping to the parent res then dividing
  ## by the child res produced a non-commensurate grid
  p_dim <- c(10L, 10L); p_ext <- c(0, 10, 0, 10)
  s <- snap_grid(c(2L, 2L), c(0.4, 4.4, 0.4, 4.4), p_dim, p_ext)
  expect_identical(s$dimension, c(2L, 2L))
  expect_equal(c(x_res(s$dimension, s$extent), y_res(s$dimension, s$extent)), c(2, 2))
  expect_true(is_aligned(p_dim, p_ext, s$dimension, s$extent))
  expect_true(is_aligned(s$dimension, s$extent, p_dim, p_ext))
})

test_that("snap_grid is a no-op on an aligned grid", {
  h <- vcrop(c(142, 158, -48, -32), g_dim, g_ext)
  s <- snap_grid(h$dimension, h$extent, g_dim, g_ext)
  expect_equal(s$extent, h$extent)
})

test_that("snap_grid rejects non-commensurate resolutions", {
  expect_error(snap_grid(c(3L, 3L), c(0, 10, 0, 10), c(10L, 10L), c(0, 10, 0, 10)),
               "not commensurate")
})

## ---- resize_grid ----

test_that("resize by integer factor", {
  h <- resize_grid(g_dim, g_ext, 2)
  expect_identical(h$dimension, c(1000L, 600L))
  expect_equal(h$extent, g_ext)
  expect_true(is_aligned(g_dim, g_ext, h$dimension, h$extent))
})

test_that("resize by fractional factor and per-axis factor", {
  h <- resize_grid(g_dim, g_ext, 0.5)
  expect_identical(h$dimension, c(250L, 150L))
  h2 <- resize_grid(g_dim, g_ext, c(2, 0.5))
  expect_identical(h2$dimension, c(1000L, 150L))
})

test_that("resize(resize(g, 2), 0.5) returns g", {
  h <- resize_grid(g_dim, g_ext, 2)
  h2 <- resize_grid(h$dimension, h$extent, 0.5)
  expect_identical(h2$dimension, g_dim)
  expect_equal(h2$extent, g_ext)
})

test_that("resize rejects a factor that breaks the lattice", {
  expect_error(resize_grid(c(10L, 10L), c(0, 10, 0, 10), 0.35), "whole-number")
  expect_error(resize_grid(c(7L, 7L), c(0, 10, 0, 10), 0.5), "whole-number")
  expect_error(resize_grid(c(10L, 10L), c(0, 10, 0, 10), -1), "positive")
})

## ---- grid_tiles ----

test_that("tiles cover the full grid exactly once", {
  tl <- grid_tiles(c(10L, 7L), c(0, 10, 0, 7), c(4L, 4L))
  expect_length(tl, 6L)
  expect_equal(sum(vapply(tl, function(t) prod(t$dimension), 1)), 70)
  ## union of extents is the parent extent
  xs <- unlist(lapply(tl, function(t) t$extent[1:2]))
  ys <- unlist(lapply(tl, function(t) t$extent[3:4]))
  expect_equal(range(xs), c(0, 10))
  expect_equal(range(ys), c(0, 7))
})

test_that("tiles are aligned to the parent and enumerated in cell order", {
  tl <- grid_tiles(g_dim, g_ext, 256L)
  for (t in tl) expect_true(is_aligned(g_dim, g_ext, t$dimension, t$extent))
  ## first tile is top-left
  expect_equal(tl[[1]]$extent[c(1, 4)], g_ext[c(1, 4)])
  expect_identical(tl[[1]]$offset, c(col = 0L, row = 0L))
  ## second tile is to the right of the first
  expect_equal(tl[[2]]$extent[1], tl[[1]]$extent[2])
})

test_that("edge tiles are smaller", {
  tl <- grid_tiles(c(10L, 7L), c(0, 10, 0, 7), c(4L, 4L))
  expect_identical(tl[[3]]$dimension, c(2L, 4L))
  expect_identical(tl[[6]]$dimension, c(2L, 3L))
})

test_that("single tile for a small grid", {
  tl <- grid_tiles(c(10L, 7L), c(0, 10, 0, 7), 256L)
  expect_length(tl, 1L)
  expect_identical(tl[[1]]$dimension, c(10L, 7L))
})

test_that("tile_dim that divides evenly leaves no edge tiles", {
  tl <- grid_tiles(c(8L, 6L), c(0, 8, 0, 6), c(4L, 2L))
  expect_length(tl, 6L)
  expect_true(all(vapply(tl, function(t) identical(t$dimension, c(4L, 2L)), logical(1))))
})

test_that("grid_tiles rejects non-positive tile_dim", {
  expect_error(grid_tiles(c(10L, 10L), c(0, 10, 0, 10), 0L), "tile_dim")
  expect_error(grid_tiles(c(10L, 10L), c(0, 10, 0, 10), c(4L, -1L)), "tile_dim")
})

## ---- invariants: symmetry, round-trips, offsets ----

test_that("is_aligned is symmetric across a range of grids", {
  cases <- list(
    list(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(2, 7, 2, 7)),
    list(c(10L, 10L), c(0, 10, 0, 10), c(2L, 2L), c(0, 10, 0, 10)),
    list(c(10L, 10L), c(0, 10, 0, 10), c(5L, 5L), c(2.5, 7.5, 2, 7)),
    list(c(10L, 10L), c(0, 10, 0, 10), c(3L, 3L), c(0, 10, 0, 10)),
    list(g_dim, g_ext, c(250L, 150L), c(145, 155, -45, -35))
  )
  for (cs in cases) {
    a <- is_aligned(cs[[1]], cs[[2]], cs[[3]], cs[[4]])
    b <- is_aligned(cs[[3]], cs[[4]], cs[[1]], cs[[2]])
    expect_identical(a, b)
  }
})

test_that("NULL extent means unit cell space", {
  expect_identical(is_aligned(c(10L, 5L), NULL, c(10L, 5L), extent0(c(10L, 5L))),
                   TRUE)
  expect_identical(grid_offset(c(10L, 5L), NULL, c(4L, 2L), c(2, 6, 1, 3)),
                   grid_offset(c(10L, 5L), extent0(c(10L, 5L)), c(4L, 2L), c(2, 6, 1, 3)))
})

test_that("resize round-trips for several factors, both orders", {
  for (k in c(2, 3, 5, 0.5)) {
    up <- resize_grid(g_dim, g_ext, k)
    back <- resize_grid(up$dimension, up$extent, 1 / k)
    expect_identical(back$dimension, g_dim)
    expect_equal(back$extent, g_ext)
  }
})

test_that("resize with a per-axis factor round-trips", {
  up <- resize_grid(g_dim, g_ext, c(2, 3))
  back <- resize_grid(up$dimension, up$extent, c(0.5, 1 / 3))
  expect_identical(back$dimension, g_dim)
  expect_equal(back$extent, g_ext)
})

test_that("snap_grid is idempotent", {
  once <- snap_grid(c(10L, 10L), c(2.3, 7.3, 2.3, 7.3), c(10L, 10L), c(0, 10, 0, 10))
  twice <- snap_grid(once$dimension, once$extent, c(10L, 10L), c(0, 10, 0, 10))
  expect_equal(twice$extent, once$extent)
  expect_identical(twice$dimension, once$dimension)
})

test_that("every tile extent is reconstructable from its offset and the parent res", {
  tl <- grid_tiles(g_dim, g_ext, c(128L, 100L))
  r <- c(x_res(g_dim, g_ext), y_res(g_dim, g_ext))
  for (t in tl) {
    off <- t$offset
    expect_equal(t$extent[1], g_ext[1] + off[["col"]] * r[1])
    expect_equal(t$extent[4], g_ext[4] - off[["row"]] * r[2])
    expect_equal(t$extent[2], t$extent[1] + t$dimension[1] * r[1])
    expect_equal(t$extent[3], t$extent[4] - t$dimension[2] * r[2])
  }
})

test_that("tol admits a sub-tolerance shift and rejects a larger one", {
  base_dim <- c(10L, 10L); base_ext <- c(0, 10, 0, 10)
  tiny <- 1e-10
  expect_true(is_aligned(base_dim, base_ext, base_dim, base_ext + c(tiny, tiny, tiny, tiny)))
  expect_false(is_aligned(base_dim, base_ext, base_dim, base_ext + c(0.01, 0.01, 0, 0)))
})
