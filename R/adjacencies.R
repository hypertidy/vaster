#' Cell adjacency
#'
#' Find neighbouring cells by index, using only the grid dimension.
#'
#' Given cell indices into a grid of known dimension, return the indices of
#' neighbouring cells. Out-of-bounds neighbours (at grid edges) are `NA`.
#'
#' The `directions` argument controls which neighbours are returned:
#'
#' - `"queen"` (default): all 8 neighbours (rook + bishop)
#' - `"rook"`: 4 cardinal neighbours (up, down, left, right)
#' - `"bishop"`: 4 diagonal neighbours
#'
#' Column order for `"queen"` is: up, down, left, right, upleft, upright,
#' downleft, downright. For `"rook"`: up, down, left, right. For `"bishop"`:
#' upleft, upright, downleft, downright. Direction names refer to the
#' raster convention where cell 1 is at the top-left.
#'
#' @section Corner vertex values from area-based rasters:
#'
#' A key use case is converting area-based cell values to corner vertex values
#' for mesh generation. Each corner vertex is shared by up to four cells.
#' For example, the top-left corner of a cell is shared with the cell above,
#' the cell to the left, and the cell diagonally above-left:
#'
#' ```
#'   upleft  |  up
#'   --------x------     vertex 'x' = mean of upleft, up, left, self
#'   left    | self
#' ```
#'
#' The queen-connected neighbours give all the cells needed to compute
#' every corner vertex. Averaging the appropriate neighbours provides
#' weighted corner values from flat pixel areas, which is the basis for
#' constructing continuous meshes from raster data (as used by quadmesh
#' and anglr).
#'
#' @param dimension integer vector of ncol, nrow
#' @param cell integer vector of cell indices (1-based)
#' @param directions character, one of `"queen"`, `"rook"`, or `"bishop"`
#'
#' @return integer matrix with one row per cell and one column per neighbour
#'   direction. Column names indicate the direction. Out-of-bounds neighbours
#'   are `NA`.
#' @export
#' @examples
#' ## 4x3 grid (4 columns, 3 rows), 12 cells
#' adjacency(c(4, 3), cell = 6)
#' adjacencyc(4, 3), cell = 6, directions = "rook")
#' adjacencyc(4, 3), cell = 6, directions = "bishop")
#'
#' ## corner cell has fewer valid neighbours
#' adjacencyc(4, 3), cell = 1)
#'
#' ## multiple cells at once
#' adjacencyc(4, 3), cell = 1:12)
#'
#' ## ------------------------------------------------
#' ## Corner vertex interpolation from area values
#' ## ------------------------------------------------
#' dm <- c(4, 3)
#' elev <- c(10, 12, 14, 16, 11, 13, 15, 17, 10, 11, 13, 14)
#'
#' ## The top-left corner of each cell is shared by self, the
#' ## cell above, the cell to the left, and the cell diagonally
#' ## above-left. Average these for the vertex value:
#' nb <- adjacency(dm, seq_along(elev))
#' vals <- cbind(elev, elev[nb[, "up"]],
#'               elev[nb[, "left"]], elev[nb[, "upleft"]])
#' corner <- rowMeans(vals, na.rm = TRUE)
#' matrix(corner, nrow = 3, byrow = TRUE)
adjacency <- function(dimension, cell, directions = "queen") {
  .check_args(dimension)
  directions <- match.arg(directions, c("queen", "rook", "bishop"))

  nc <- dimension[1L]
  nr <- dimension[2L]

  row <- row_from_cell(dimension, cell)
  col <- col_from_cell(dimension, cell)

  ## helper: row,col to cell with NA for out-of-bounds
  rc2cell <- function(r, c) {
    bad <- r < 1L | r > nr | c < 1L | c > nc
    idx <- cell_from_row_col(dimension, r, c)
    idx[bad] <- NA_integer_
    idx
  }

  rook <- cbind(
    up    = rc2cell(row - 1L, col),
    down  = rc2cell(row + 1L, col),
    left  = rc2cell(row, col - 1L),
    right = rc2cell(row, col + 1L)
  )

  bishop <- cbind(
    upleft    = rc2cell(row - 1L, col - 1L),
    upright   = rc2cell(row - 1L, col + 1L),
    downleft  = rc2cell(row + 1L, col - 1L),
    downright = rc2cell(row + 1L, col + 1L)
  )

  switch(directions,
         rook   = rook,
         bishop = bishop,
         queen  = cbind(rook, bishop)
  )
}
