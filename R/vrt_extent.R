#' Extents from VRT
#'
#' Get extent from index values in VRT text.
#'
#' (I can't understand XML tech for the life of me so I hack the text as lines with strsplit)
#' @param x url or file path to VRT file
#'
#' @export
#' @examples
#' src <- "https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt"
#' src <- "https://opentopography.s3.sdsc.edu/raster/SRTM_GL1/SRTM_GL1_srtm.vrt"
#' ex <- extent_vrt(src)
#' op <- par(mar = rep(0, 4))
#' plot(range(ex[,1:2]), range(ex[,3:4]), xlab = "", ylab = "", asp = "", type = "n")
#' rect(ex[,1], ex[,3], ex[, 2], ex[,4])
#' par(op)
extent_vrt <- function(x) {
  vrt <- readr::read_lines(x)

  vrt1 <- grep("DstRect xOff=\"", vrt, value = TRUE)
  xOff <- as.integer(unlist(lapply(strsplit(unlist(lapply(strsplit(vrt1, "DstRect xOff=\""), "[", 2)), "\" yOff"), "[", 1)))
  yOff <- as.integer(unlist(lapply(strsplit(unlist(lapply(strsplit(vrt1, "yOff=\""), "[", 2)), "\".*xSize"), "[", 1)))

  xSize <- as.integer(unlist(lapply(strsplit(unlist(lapply(strsplit(vrt1, "xSize=\""), "[", 2)), "\".*ySize"), "[", 1)))
  ySize <- as.integer(unlist(lapply(strsplit(unlist(lapply(strsplit(vrt1, "ySize=\""), "[", 2)), "\""), "[", 1)))


  vinfo <- vapour::vapour_raster_info(x)
  xmin <- vaster::x_from_col(vinfo$extent, vinfo$dimXY, xOff + 1)
  xmax <- vaster::x_from_col(vinfo$extent, vinfo$dimXY, xOff + xSize)
  ymin <- vaster::y_from_row(vinfo$extent, vinfo$dimXY, yOff + 1)
  ymax <- vaster::y_from_row(vinfo$extent, vinfo$dimXY, yOff +  ySize)
  cbind(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
