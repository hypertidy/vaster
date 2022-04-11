
# vaster

<!-- badges: start -->
<!-- badges: end -->

The goal of vaster is to ...


This is *very* barebones, while I work on related things. 

There's affinity, vapour, gdalio, and granulated - all having a say in this. I want this basically to do 

```R
r <- raster()
r1 <- crop(r, extent(xmin, xmax, ymin, ymax))
```

*without* all the unnecessary spatial guff. (It does what raster's alignExtent does, and what raster/stars/terra/spatstat/GDAL and your GIS all do with a discretized domain). 

I might use this to create VRT templates for some cases, and possibly we should have rasterio-alike affine support in this package. 

get in touch IYI


