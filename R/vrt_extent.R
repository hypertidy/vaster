#' Extents from VRT
#'
#' Get extent from index values in VRT text.
#'
#' (I can't understand XML tech  so I hack the text as lines with strsplit)
#' @param x url or file path to VRT file
#'
#' @export
#' @examples
#' #src <- "https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt"
#' #src <- "https://opentopography.s3.sdsc.edu/raster/SRTM_GL1/SRTM_GL1_srtm.vrt"
#' #ex <- extent_vrt(src)
#' #op <- par(mar = rep(0, 4))
#' #plot(range(ex[,1:2]), range(ex[,3:4]), xlab = "", ylab = "", asp = "", type = "n")
#' #rect(ex[,1], ex[,3], ex[, 2], ex[,4])
#' #par(op)
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


show_tiling <- function(overviews, extent) {

}
