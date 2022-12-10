## vast
## is basically raster()/terra() but much simpler

## features
## slots for extent, dimension, crs, sds, bands, interpretation (bands of double, hex character, bands of Byte, etc)
## can be empty or be a character vector of dsn
## when being *materialized* as a data source in some way, the extent, crs, and other details

## one problem is how to flag that the extent/crs etc are to override the source, but let's worry later

vast <- function(dimension = c(1L, 1L), extent = NULL, crs = NA_character_,
                 sds = NA, bands = NA, interpretation = NA, sources = character()) {

  ## this gdalio::vrt() but also see mdsumner/place and
  if (is.null(extent)) extent <- c(0, dimension[1L], 0, dimension[2L])
  structure(sources, dimension = dimension, extent = extent, crs = crs,
            sds = sds, bands = bands, interpretation =  interpretation,
            class = c("vast", "character"))

}
