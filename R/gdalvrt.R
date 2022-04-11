etopo2 <- function(local = TRUE) {
  f <- "/rdsi/PUBLIC/raad/data/www.ngdc.noaa.gov/mgg/global/relief/ETOPO2/ETOPO2v2-2006/ETOPO2v2c/netCDF/ETOPO2v2c_f4.nc"
  if (!file.exists(f) || !local) {
    f <- "/vsizip//vsicurl/https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO2/ETOPO2v2-2006/ETOPO2v2c/netCDF/ETOPO2v2c_f4_netCDF.zip"
  }
  f
}

filepath <- etopo2()


basicvrt <- function(filepath, extent = NULL, dimension = NULL, projection = NULL, data_type = NULL, bands = 1L) {
  if (is.null(extent)) {
    message("no extent provided, guessing -180,180,-90,90")
    extent <- c(-180, 180, -90, 90)
  }
  if (is.null(dimension)) {
    message("no dimension provided, guessing 360,180")
    dimension <- c(360, 180)
  }
  if (is.null(data_type)) {
    message("no data_type provided, guessing Float32")
    data_type = "Float32"
  }
  if (is.null(projection)) {
    message("no projection provided, guessing OGC:CRS84")
    projection <- "OGC:CRS84"
  }
  geotransform <- affinity::extent_dim_to_gt(extent, dimension)
  ## how GDAL formats it
  geotransform_format <- sprintf("%24.16e,%24.16e,%24.16e,%24.16e,%24.16e,%24.16e",
                                geotransform[1L],
                                geotransform[2L],
                                geotransform[3L],
                                geotransform[4L],
                                geotransform[5L],
                                geotransform[6L])

  nbands <- length(bands)
  if (any(bands < 1)) stop("bands must be values of 1 or above")
  bandlist <- character(nbands)
  for (iband in seq_len(nbands)) {
  bandlist[iband] <-  glue::glue( '
    <VRTRasterBand dataType="{data_type}" band="{iband}">
      <NoDataValue>nan</NoDataValue>
      <SimpleSource>
      <SourceFilename relativeToVRT="0">{filepath}</SourceFilename>
        <SourceBand>{bands[iband]}</SourceBand>
        </SimpleSource>
        </VRTRasterBand>
        ')
  }
bandlist_format <- paste(bandlist, collapse = "\n")
glue::glue(
'
<VRTDataset rasterXSize="{dimension[1L]}" rasterYSize="{dimension[2L]}">
  <SRS dataAxisToSRSAxisMapping="1,2">{projection}</SRS>
  <GeoTransform>{geotransform_format} </GeoTransform>
{bandlist_format}
</VRTDataset>
'
)

}

#' vrtmaker <- function() {
#'   vrtband()
#'
#' }
#'
#' #' gdalvrt(etopo2(), source_extent = c(-180, 180, -90, 90), source_projection = "OGC:CRS84")
#' gdalvrt <- function(dsn,  ...,
#'                     source_extent = NULL, source_projection = NULL, sds = 1L) {
#'
#' }
