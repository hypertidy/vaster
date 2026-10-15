#' Extents from VRT (virtual raster data set of GDAL)
#'
#' Get extent from index values in VRT text, these are the individual footprints of raster
#' windows in a VRT. These can be arbitrary to a specific grid, but generally are used for tiled mosaics.
#'
#' Each VRT raster element records it's relative position within the grid, so we grid logic
#' to expand the actual extent of each element, and return those as a matrix of xmin,xmax,ymin,ymax.
#'
#' The VRT XML is parsed with the \pkg{xml2} package, which must be installed
#' (it is a suggested dependency).
#'
#' @param x url, file path, or text connection to a VRT file
#'
#' @returns a matrix of 4 columns, xmin,xmax,ymin,ymax
#' @export
#' @examplesIf requireNamespace("xml2", quietly = TRUE)
#' #src <- "https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt"
#' src <- gzfile(system.file("extdata/NASADEM_be.vrt.gz", package = "vaster"), "rt")
#' ## read VRT from a URL or file (we use a connection here to keep package example small)
#' ex <- extent_vrt(src)
#' close(src)
#' plot_extent(ex)
extent_vrt <- function(x) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("extent_vrt() requires the 'xml2' package; install it with install.packages('xml2')",
         call. = FALSE)
  }
  ## read_xml() takes a path or URL directly; for a connection read the text
  ## first (parsed as a literal document via raw bytes)
  if (inherits(x, "connection")) {
    doc <- xml2::read_xml(charToRaw(enc2utf8(paste(readLines(x), collapse = "\n"))))
  } else {
    doc <- xml2::read_xml(x)
  }
  root <- xml2::xml_root(doc)

  dimension <- as.integer(c(xml2::xml_attr(root, "rasterXSize"),
                            xml2::xml_attr(root, "rasterYSize")))
  geot <- as.numeric(strsplit(xml2::xml_text(xml2::xml_find_first(root, ".//GeoTransform")),
                              ",")[[1]])
  xx <- c(geot[1], geot[1] + dimension[1] * geot[2])
  yy <- c(geot[4] + dimension[2] * geot[6], geot[4])
  extent <- c(xx, yy)

  dst <- xml2::xml_find_all(root, ".//DstRect")
  xOff <- as.numeric(xml2::xml_attr(dst, "xOff"))
  yOff <- as.numeric(xml2::xml_attr(dst, "yOff"))
  xSize <- as.numeric(xml2::xml_attr(dst, "xSize"))
  ySize <- as.numeric(xml2::xml_attr(dst, "ySize"))

  ## cell centres of the first and last col/row, pushed out by half a cell to the edges
  ## (rows count from the top, so the first row gives ymax)
  xres <- x_res(dimension, extent)
  yres <- y_res(dimension, extent)
  xmin <- x_from_col(dimension, extent, xOff + 1)     - xres / 2
  xmax <- x_from_col(dimension, extent, xOff + xSize) + xres / 2
  ymax <- y_from_row(dimension, extent, yOff + 1)     + yres / 2
  ymin <- y_from_row(dimension, extent, yOff + ySize) - yres / 2
  cbind(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

