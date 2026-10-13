<!-- Copied from mdsumner/align (Feb 2026) on 2026-10-13 when the alignment-prison functions were folded into vaster as R/lattice.R: is_aligned(), grid_offset(), snap_grid(), resize_grid(), grid_tiles(). The design below predates that and still refers to a separate vast object and package; the ideas are current, the package layout is not. -->

# align: a grid specification primitive for R

working labels: align, step, vast

working naming: we are stealing names from terra and vaster and not worrying about that yet

## What it is

align is a pure R package that provides a single core object: a grid specification defined by dimension and extent. Everything else — resolution, coordinates, cell indices, row/column mappings — is derived on demand by arithmetic.

The grid spec is a window onto an infinite regular lattice. Crop, expand, and resize operations produce new specs on the same lattice (or an integer-related one). Content — coverage fractions, raster values, sparse indexes — references the grid by index and is not part of the grid itself.

## The object

```r
g <- vast(c(100L, 200L))
# dimension: 100 x 200 (ncol x nrow)
# extent: c(0, 100, 0, 200)
# resolution: 1 x 1

g <- vast(c(500L, 300L), extent = c(140, 160, -50, -30))
# dimension: 500 x 300
# extent: c(140, 160, -50, -30)
# resolution: 0.04 x 0.0667

g <- vast_res(extent = c(140, 160, -50, -30), resolution = 0.04)
# dimension derived: c(500, 500)
# equivalent to: vast(ceiling(c(20, 20) / 0.04), extent)
```

### Storage

Two values, always present:

- `dimension`: integer vector, `c(ncol, nrow)`
- `extent`: numeric vector, `c(xmin, xmax, ymin, ymax)`

That's it. No resolution field, no CRS, no data, no cell values.

### Alignment enforced

(see Aliases below)

Dimension is canonical, immutable at first input  as is extent. (Caveat for actual numeric limitations)

Resolution is derived from extent subdivided by dimension.

Resolution as input is not compatible as (dimension, resolution) - incomplete specification needs two offset anchors. Helpers should exist for various inputs, affine, (extent,resolution) but always
something is modified - extent or resolution have to snap.

Crop and Expand are conceptually different, but are complementary and symmetric, and can be no-op. If we have relative crop `* .9` or `- 10` we also have `/ .9` or `+10` relative expand (terra does this, but I'm not sure we'd choose the same logic and I'm not fully across it, needs discussion). I don't think we have a free-floating extent, it's always bound to a discretization (dimension), and can be 1,1.


### Default extent

```r
vast(c(100L, 200L))
# extent defaults to c(0, ncol, 0, nrow) — unit cell space
```

This gives an identity mapping where column index = x coordinate and row index = y coordinate. The unit grid is pure index space with no geographic pretension.

### Derived quantities

All computed, never stored:

```r
ncol(g)          # dimension[1]
nrow(g)          # dimension[2]
ncell(g)         # prod(dimension)
res(g)           # c(dx, dy) = c(diff(extent[1:2]) / ncol, diff(extent[3:4]) / nrow)

# coordinate vectors — sequences, not stored arrays
x(g)             # xmin + (seq_len(ncol) - 0.5) * dx  (cell centres)
y(g)             # ymax - (seq_len(nrow) - 0.5) * dy  (cell centres, top to bottom)

# index <-> coordinate conversions
cell_from_xy(g, xy)          # integer cell index (1-based)
xy_from_cell(g, cell)        # 2-column matrix
cell_from_rowcol(g, row, col)
rowcol_from_cell(g, cell)
row_from_y(g, y)
col_from_x(g, x)
x_from_col(g, col)
y_from_row(g, row)
```

These are all one-line arithmetic expressions. No lookup tables, no allocations beyond the result vector.

## Operations

### Crop

Restrict the extent. Snaps to cell boundaries of the parent grid.

```r
crop(g, extent)
# 1. snap requested extent inward to nearest cell boundaries
# 2. compute new dimension from snapped extent / resolution
# 3. return vast(new_dim, snapped_extent)
```

The resolution doesn't change. The new grid is a subwindow of the old one. Row/column offsets between parent and child are exact integers.

```r
child <- crop(g, c(141, 155, -45, -35))
offset(child, g)
# c(col_off, row_off) — integer offset from parent origin to child origin
```

### Expand

Widen the extent. The inverse of crop — snaps outward to cell boundaries.

```r
expand(g, extent)
# snap outward, compute new dimension
```

expand + crop are symmetric: `crop(expand(g, e), g$extent)` returns `g`.

### Buffer

'buffer' with positive and negative behaviour might be a better scheme than crop and expand.

### Resize

Change resolution by an integer factor. The new lattice nests exactly within the old one.

```r
resize(g, factor = 2L)
# double resolution: dim * 2, extent unchanged
# every old cell becomes a 2x2 block

resize(g, factor = 0.5)
# halve resolution: dim / 2, extent unchanged
# every 2x2 block becomes one cell
# dimension must be divisible by 1/factor
```

`factor` must produce exact integer dimensions. This is the alignment invariant — you can't resize to a resolution that breaks the lattice.

### Align check

Test whether two grids share a lattice (same origin modulo resolution, commensurate resolutions).

```r
is_aligned(g1, g2)
# TRUE if g2's cell boundaries fall exactly on g1's cell boundaries
# (or vice versa)

snap(g, parent)
# snap g's extent to parent's cell boundaries
# error if resolutions are not commensurate
```

### Tile

Partition a grid into subwindows.

```r
tiles(g, tile_dim = c(256L, 256L))
# returns a list of vast objects, each tile_dim or smaller (edge tiles)
# all aligned to g
```

This is pure index arithmetic — how many tiles fit, what are their extents. No data, no I/O.

## What align does NOT do

- Store raster values
- Read or write files
- Handle CRS or projection (we at some point store it - 'carry' - I think that was a mistake for terra, to not)
- Do geometry operations
- Allocate large arrays
- n-dimensions

align is six numbers and arithmetic on them. It validates alignment and computes indices. That's the entire scope.


## Type and class

Start with a named list and an S3 class:

```r
vast <- function(dimension, extent = NULL) {
  dimension <- as.integer(dimension)
  stopifnot(length(dimension) == 2L, all(dimension > 0L))

  if (is.null(extent)) {
    extent <- c(0, dimension[1], 0, dimension[2])
  }
  extent <- as.double(extent)
  stopifnot(length(extent) == 4L, extent[2] > extent[1], extent[4] > extent[3])

  structure(
    list(dimension = dimension, extent = extent),
    class = "vast"
  )
}
```

Consider S7 later if method dispatch gets complex. For now, S3 is fine — the operations are functions on the type, not a class hierarchy.

### Print

```r
print.vast <- function(x, ...) {
  r <- res(x)
  cat(sprintf("<vast> %d x %d (%.6g x %.6g)\n",
      x$dimension[1], x$dimension[2], r[1], r[2]))
  cat(sprintf("  x: [%g, %g]\n  y: [%g, %g]\n",
      x$extent[1], x$extent[2], x$extent[3], x$extent[4]))
  invisible(x)
}
```

## Aliases

```r
vast_res <- function(extent, resolution) {
  extent <- as.double(extent)
  resolution <- as.double(rep_len(resolution, 2L))
  dimension <- ceiling(c(
    diff(extent[1:2]) / resolution[1],
    diff(extent[3:4]) / resolution[2]
  ))
  # adjust extent to honour exact resolution
  extent[2] <- extent[1] + dimension[1] * resolution[1]
  extent[4] <- extent[3] + dimension[2] * resolution[2]
  vast(dimension, extent)
}
```

Other entry points that call `vast()`:

```r
# from a terra/raster object
as_vast <- function(x, ...) UseMethod("as_vast")
as_vast.SpatRaster <- function(x, ...) {
  vast(c(ncol(x), nrow(x)), as.vector(terra::ext(x))[c(1,2,3,4)])
}

# from controlledburn output
as_vast.controlledburn <- function(x, ...) {
  vast(x$dimension, x$extent)
}
```

## Relationship to other packages

### controlledburn

controlledburn produces sparse runs + edges on a grid. `materialise_chunk()` needs to:
1. Accept a target `vast` object (subwindow of the parent grid)
2. Validate alignment via `is_aligned()`
3. Compute row/col offset via `offset()`
4. Filter and reindex runs/edges
5. Fill a matrix of the target's dimensions

align provides steps 1–4. controlledburn does step 5.

### vaster

vaster currently implements cell-from-xy, extent alignment, dimension fitting. The core of that logic moves into align as the `vast` object and its methods. vaster becomes a higher-level package that imports align and adds convenience for terra/raster/stars interop.

### grout

grout handles tile schemes and validates spatial alignment for cloud-native raster workflows. Its tile partitioning logic (`tiles()`) belongs in or delegates to align. grout adds the I/O coordination — which tiles exist, what format, where.

## tripEstimation / SGAT

These packages have versions or vestiges of child-parent window scheme designed in 2003, called "Pimage" in tripEstimation. (https://figshare.utas.edu.au/ndownloader/files/40906604 Chapter 5).

This project will be a perfect fit for that application, [SGATII](https://github.com/SWotherspoon/SGATII) is the successor project.

### DuckDB / arrow workflows

The sparse output from controlledburn is a data frame. Index remapping (crop/resize) on that data frame is row filtering + integer arithmetic on the index columns. align provides the grid spec and the arithmetic; the query engine (DuckDB, arrow, data.table) provides the execution.

```r
parent <- as_vast(burn_result)
tile   <- crop(parent, tile_extent)
off    <- offset(tile, parent)

# pure index operation — works in DuckDB, arrow, or base R
runs |>
  filter(row >= off[2] + 1, row <= off[2] + nrow(tile)) |>
  filter(col_end >= off[1] + 1, col_start <= off[1] + ncol(tile)) |>
  mutate(
    row = row - off[2],
    col_start = pmax(col_start - off[1], 1L),
    col_end = pmin(col_end - off[1], ncol(tile))
  )
```

## Resolution collapse

Given a controlledburn object on a parent grid and a target grid at coarser resolution (integer multiple), produce a new controlledburn object without materialising either grid.

```r
# parent: 1000 x 1000
# target: 100 x 100 (10x coarser, same extent)

parent_grid <- as_vast(burn_result)
target_grid <- resize(parent_grid, factor = 0.1)

# interior runs: divide col_start/col_end by 10, merge adjacent
# edge cells at run boundaries: aggregate coverage within 10x10 blocks
# interior of runs at coarse scale: still 1.0, no computation
# result: new controlledburn object on target_grid
```

The vast majority of cells (interior runs) collapse by dividing two integers. Only the O(perimeter) boundary cells need actual aggregation. This is pyramid generation from the sparse format.

align provides `resize()` and the block-mapping arithmetic. The aggregation logic (how to combine coverage fractions within a block) lives in controlledburn or in a general sparse-index-collapse package.

## Implementation plan

1. `vast()` constructor + validation + print
2. `res()`, `ncol()`, `nrow()`, `ncell()`
3. `x()`, `y()`, coordinate vectors
4. `cell_from_xy()`, `xy_from_cell()`, `cell_from_rowcol()`, `rowcol_from_cell()` — the vaster core
5. `crop()`, `expand()` with snap-to-cell
6. `offset()` — integer displacement between aligned grids
7. `is_aligned()`, `snap()`
8. `resize()` with integer-factor validation
9. `tiles()` — partition into subwindows
10. `vast_res()` and `as_vast()` methods
11. Update controlledburn `materialise_chunk()` to accept a `vast` target
12. Migrate vaster's core index arithmetic to align

All pure R. No compiled code. No dependencies beyond base R (wk is not needed — align has no geometry). The test suite is arithmetic identities — round-trip conversions, crop/expand symmetry, alignment invariants.

## The alignment prison

The invariant that makes everything work: two `vast` objects are *compatible* if and only if their cell boundaries coincide. This means:

1. Same origin (modulo resolution): `(extent[1] - other$extent[1]) %% dx == 0`
2. Commensurate resolution: one resolution is an exact integer multiple of the other

If these hold, every index operation (crop, offset, resize, tile, reindex) produces exact integer results. No floating-point negotiation, no half-cell shifts, no off-by-one ambiguity.

If they don't hold, align refuses to proceed. The type system prevents misaligned operations by construction. That's the prison — and it's the entire value of the package.
