## code to prepare `DATASET` dataset goes here

## only in non- git managed and no R-built context
library(raadtools)
library(vaster)
r <- terra::rast(topofile("etopo2"))
#writeRaster(r, "inst/misc/etopo2.tif", gdal = list("COMPRESS=DEFLATE"), filetype = "COG")
#system("gdalinfo inst/misc/etopo2.tif")
#Band 1 Block=512x512 Type=Float32, ColorInterp=Gray
#Overviews: 5400x2700, 2700x1350, 1350x675, 675x337, 337x168
