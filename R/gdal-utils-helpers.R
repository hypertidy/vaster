#' Target size and extent for GDAL command line
#'
#' Format grid properties for GDAL command line options (`-ts` for target size,
#' `-te` for target extent).
#'
#' These functions generate the string arguments used by GDAL utilities like
#' `gdalwarp` and `gdal_translate`. The `ts()` function is named after the GDAL
#' `-ts` flag and masks [stats::ts()]; use `ts_te()` or qualify with
#' `stats::ts()` if you need the time series function.
#'
#' @param dimension integer vector of ncol, nrow (target size)
#' @param extent numeric vector of xmin, xmax, ymin, ymax (target extent)
#'
#' @return A character string formatted for GDAL command line:
#'
#'
#' - `ts_te()`: combined `-ts` and `-te` arguments
#' - `ts()`: `-ts ncol nrow` string
#' - `te()`: `-te xmin ymin xmax ymax` string (note: reordered for GDAL)
#'
#' @export
#' @name ts_te
#' @seealso [vcrop()] for computing aligned extents
#'
#' @examples
#' ts_te(c(10, 100), 1:4)
#'
#' ts(c(10, 100))
#'
#' te(1:4)
#'
#' ## use in a GDAL command (not run)
#' \dontrun{
#' cmd <- sprintf("gdalwarp %s %s input.tif output.tif",
#'                ts(c(1000, 500)), te(c(-180, 180, -90, 90)))
#' }
ts_te <- function(dimension, extent) {
  paste0(ts(dimension), " ", te(extent))
}

#' @rdname ts_te
#' @export
te <- function(extent) {
  sprintf("-te %s", paste0(extent[c(1, 3, 2, 4)], collapse = " "))
}

#' @rdname ts_te
#' @export
#' @note `ts()` masks [stats::ts()]. This is intentional for concise GDAL
#'   command building, but be aware of the conflict if you use time series
#'   in the same session.
ts <- function(dimension) {
  sprintf("-ts %s", paste0(dimension[1:2], collapse = " "))
}
