# vaster

The goal of vaster is to provide grid logic without complication added
by data and format details.

## Installation

You can install vaster from
[r-universe](https://hypertidy.r-universe.dev):

``` r

install.packages("vaster", repos = c("https://hypertidy.r-universe.dev", "https://cran.r-project.org"))
```

Or install the development version from
[GitHub](https://github.com/hypertidy/vaster):

``` r

# install.packages("pak")
pak::pak("hypertidy/vaster")
```

## Grid logic

Here are four numbers, that represent two positions in the plane.

On their own they are very unlikely to align on a raster grid, but they
provide a starting point for a “region of interest”. Notice in this
picture that the underlying discretization and the position of these
points is unrelated, and can be arbitrary - but they are related in
terms of describing *approximately* the same region. If we make our grid
window too small we will miss part of the world that the points are in,
you can see that we want to “snap out” at least one grid cell to the
left and down, to the right and up.

``` r

library(vaster)
set.seed(1)
x <- c(sort(runif(2, -20, 160)), sort(runif(2, -80, 10)))
names(x) <- c("xmin", "xmax", "ymin", "ymax")
x
#>       xmin       xmax       ymin       ymax 
#>  27.791559  46.982302 -28.443197   1.738701

## a shared palette for the plots below
col_fore <- "#1C6E8C"
col_roi <- "firebrick"
col_grid <- "grey85"

op <- par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0), bty = "n")
## pad the view so the corner labels have room
padx <- diff(range(x[1:2])) * 0.2
pady <- diff(range(x[3:4])) * 0.2
plot(x[1:2], x[3:4], type = "n", asp = 1, xlab = "x", ylab = "y",
     xlim = range(x[1:2]) + c(-padx, padx),
     ylim = range(x[3:4]) + c(-pady, pady))
## an arbitrary lattice the points know nothing about
abline(v = seq(-40, 180, by = 20), h = seq(-100, 40, by = 20), col = col_grid)
## the region of interest implied by the two points
rect(x[1], x[3], x[2], x[4], border = col_roi, lty = 2, lwd = 2)
points(x[1:2], x[3:4], pch = 19, col = col_roi)
text(x[1], x[3], "(xmin, ymin)", pos = 1, col = col_roi, cex = 0.8)
text(x[2], x[4], "(xmax, ymax)", pos = 3, col = col_roi, cex = 0.8)
```

![Two points on an arbitrary background lattice, joined by a dashed red
rectangle marking the region of interest they
imply.](reference/figures/README-vaster-1.png)

``` r

par(op)
```

This is where vaster comes in, tools to relate arbitrary point locations
to an underlying grid.

vaster consists of a set of low-level functions for this relationship
between point locations and grid elements.

The [`vcrop()`](https://hypertidy.github.io/vaster/reference/vcrop.md)
function is a high-level function that uses these underlying tools.
Imagine a grid that is 120x60 and includes the whole world in longlat,
this is described by dimension (120x60) and by extent (-180, 180, -90,
90). With
[`vcrop()`](https://hypertidy.github.io/vaster/reference/vcrop.md) we
get exactly the right snapping for our points, identifying the exact
extent we need to “snap out” that little bit as mentioned above. The
discretization can be completely arbitrary, as it can be in geospatial
packages.

``` r

## all we need is an extent and dimension, we want to align to that grid
v <- vcrop(x, c(120, 60), extent = c(-180, 180, -90, 90))
g_along <- function(x, n) seq(x[1], x[2], length.out = n + 1)

op <- par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0), bty = "n")
plot(NA, xlim = v$extent[1:2], ylim = v$extent[3:4], asp = 1, xlab = "x", ylab = "y")
abline(v = g_along(v$extent[1:2], v$dimension[1]),
       h = g_along(v$extent[3:4], v$dimension[2]), col = col_grid)
## the grid-aligned extent vcrop() returns
rect(v$extent[1], v$extent[3], v$extent[2], v$extent[4], border = col_fore, lwd = 2)
## the points cropped the grid; they don't define its alignment
rect(x[1], x[3], x[2], x[4], border = col_roi, lty = 2, lwd = 2)
points(x[1:2], x[3:4], pch = 3, col = col_roi)
legend("topleft", inset = 0.02, bty = "n", cex = 0.9,
       legend = c("requested", "snapped (vcrop)"),
       col = c(col_roi, col_fore), lty = c(2, 1), lwd = 2)
```

![The requested region (dashed red) and the grid-aligned extent vcrop()
snaps outward to (teal), over the world grid
cells.](reference/figures/README-vaster1-1.png)

``` r

par(op)
```

That is the core of vaster - this is *raster logic* that exists in
geospatial libraries and packages, but it has use on its own without any
imagery, or grid data, or files or anything else. vaster was originally
based on the abstract cell functions of the old
[raster](https://CRAN.R-project.org/package=raster), which relied on its
internal format - these are now superseded by the
[terra](https://CRAN.R-project.org/package=terra) package - but we don’t
want to rely on its new format, or be subject to future changes so
vaster has the logic separated from other tooling.

Most functions in vaster include arguments that ‘dimension’, and
‘extent’ (when needed) in that order.

We use the concept of “extent” which is xmin,xmax,ymin,ymax which is
exactly the same as the concept of “bounding box (bbox)” which is
xmin,ymin,xmax,ymax: the same thing in a different order.

## Another example

A grid of the world in 9x5 degree squares, we only need the dimension
and extent to get the corners in x, y.

With
[`plot_extent()`](https://hypertidy.github.io/vaster/reference/plot_extent.md)
we have a very simple way to plot a rectangular region with one
argument.

``` r

ex <- c(-180, 180, -90, 90)
dm <- c(40, 36)

op <- par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0), bty = "n")
plot_extent(ex, border = NA)
abline(v = x_corner(dm, ex), h = y_corner(dm, ex), col = col_grid)
plot_extent(ex, border = col_fore, lwd = 2, add = TRUE)
```

![A 40 by 36 world grid: faint grey cell boundaries inside a teal extent
outline.](reference/figures/README-world-1.png)

``` r

par(op)
```

With
[`x_corner()`](https://hypertidy.github.io/vaster/reference/coordinates.md),
[`y_corner()`](https://hypertidy.github.io/vaster/reference/coordinates.md),
[`x_centre()`](https://hypertidy.github.io/vaster/reference/coordinates.md),
and
[`y_centre()`](https://hypertidy.github.io/vaster/reference/coordinates.md)
we need only those six numbers, extent and dimension to get the corner
and cell centre positions.

Now add the centre points.

``` r

op <- par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0), bty = "n")
plot_extent(ex, border = NA)
abline(v = x_corner(dm, ex), h = y_corner(dm, ex), col = col_grid)
plot_extent(ex, border = col_fore, lwd = 2, add = TRUE)
points(x_centre(dm, ex), rep(y_centre(dm, ex)[1], length.out = dm[1]),
       pch = 19, cex = 0.6, col = col_fore)
```

![The world grid with the cell-centre points of the top row marked in
teal.](reference/figures/README-points-1.png)

``` r

par(op)
```

We only get the margins from x_corner/x_centre so we go to a cell based
function,
[`xy_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.md)
returns any centre position indexed by its cell number (cell numbers are
1-based and traverse from topleft to the right, then down by row - this
is the same as in raster/terra and is the orientation used by most
geospatial tools).

``` r

## how many cells?
cells <- seq_len(prod(dm))
op <- par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0), bty = "n")
plot_extent(ex, border = col_fore, lwd = 2)
points(xy_from_cell(dm, ex, cells), pch = 19, cex = 0.3, col = col_fore)
```

![Every cell-centre point of the world grid, drawn as small teal dots
inside the extent.](reference/figures/README-cells-1.png)

``` r

par(op)
```

Other functions return cells, here we have another kind of “snap”
operation, which cell does the point fall it (or which cell centre is it
closest to).

``` r

set.seed(4827)
xy <- cbind(runif(50, -180, 180), runif(50, -90, 90))
cells <- cell_from_xy(dm, ex, xy)
ctr <- xy_from_cell(dm, ex, cells)

op <- par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0), bty = "n")
plot_extent(ex, border = NA)
abline(v = x_corner(dm, ex), h = y_corner(dm, ex), col = col_grid)
plot_extent(ex, border = col_fore, lwd = 2, add = TRUE)
## each query point "snaps" to its cell centre
segments(xy[, 1], xy[, 2], ctr[, 1], ctr[, 2], col = "grey60")
points(ctr, pch = 19, cex = 0.7, col = col_fore)
points(xy, pch = 3, col = col_roi)
legend("bottomleft", inset = 0.02, bty = "n", cex = 0.9,
       legend = c("query point", "cell centre"),
       col = c(col_roi, col_fore), pch = c(3, 19))
```

![Random query points (red crosses) each joined by a line to the centre
of the cell they fall in (teal
dots).](reference/figures/README-query-1.png)

``` r

par(op)
```

## Code of Conduct

Please note that the vaster project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
