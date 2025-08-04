curl::curl_download("https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt",
                    "inst/extdata/NASADEM_be.vrt")

gz <- gzfile("inst/extdata/NASADEM_be.vrt.gz", "w")
lines <- readLines("inst/extdata/NASADEM_be.vrt")
writeLines(lines, gz)
close(gz)
unlink("inst/extdata/NASADEM_be.vrt")
