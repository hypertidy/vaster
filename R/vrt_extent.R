#' Extents from VRT (virtual raster data set of GDAL)
#'
#' Get extent from index values in VRT text, these are the individual footprints of raster
#' windows in a VRT. These can be arbitrary to a specific grid, but generally are used for tiled mosaics.
#'
#' Each VRT raster element records it's relative position within the grid, so we grid logic
#' to expand the actual extent of each element, and return those as a matrix of xmin,xmax,ymin,ymax.
#'
#'
#' @param x url or file path to VRT file
#'
#' @returns a matrix of 4 columns, xmin,xmax,ymin,ymax
#' @export
#' @examples
#' #src <- "https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt"
#' src <- gzfile(system.file("extdata/NASADEM_be.vrt.gz", package = "vaster"), "rt")
#' ## read VRT from a URL or file (we use a connection here to keep package example small)
#' ex <- extent_vrt(src)
#' close(src)
#' plot_extent(ex)
extent_vrt <- function(x) {
  vrt <- readLines(x)

  dimension <- as.integer(c(strsplit(strsplit(vrt[1], "rasterXSize=\"")[[1]][2], "\"")[[1]][1],
                 strsplit(strsplit(vrt[1], "rasterYSize=\"")[[1]][2], "\"")[[1]][1]))
  geot <- as.numeric(unlist(strsplit(gsub("</GeoTransform>", "", gsub("<GeoTransform>", "",
                                           grep("GeoTransform", vrt, value = TRUE))), ",")))
  xx <- c(geot[1], geot[1] + dimension[1] * geot[2])
  yy <- c(geot[4] + dimension[2] * geot[6], geot[4])
  extent <- c(xx, yy)

  vrt1 <- grep("DstRect xOff=\"", vrt, value = TRUE)
  xOff <- as.integer(unlist(lapply(strsplit(unlist(lapply(strsplit(vrt1, "DstRect xOff=\""), "[", 2)), "\" yOff"), "[", 1)))
  yOff <- as.integer(unlist(lapply(strsplit(unlist(lapply(strsplit(vrt1, "yOff=\""), "[", 2)), "\".*xSize"), "[", 1)))

  xSize <- as.integer(unlist(lapply(strsplit(unlist(lapply(strsplit(vrt1, "xSize=\""), "[", 2)), "\".*ySize"), "[", 1)))
  ySize <- as.integer(unlist(lapply(strsplit(unlist(lapply(strsplit(vrt1, "ySize=\""), "[", 2)), "\""), "[", 1)))


  xmin <- vaster::x_from_col(dimension, extent, xOff + 1)
  xmax <- vaster::x_from_col(dimension, extent, xOff + xSize)
  ymin <- vaster::y_from_row(dimension, extent, yOff + 1)
  ymax <- vaster::y_from_row(dimension, extent, yOff +  ySize)
  cbind(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

