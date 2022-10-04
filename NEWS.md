# vaster dev

* New function `snap_extent()` to replace `spex::buffer_extent()`, aliased to `buffer_extent()` in this package. 

* Better argument consistency, dimension first, then extent. 

* New function `extent_vrt()` to obtain tile extents from VRT text. 

* New function `vaster_listxyz()` to convert extent, dimension, and optionally data to trad `image()` format. 


* New function `vaster_long()` to convert extent, dimension, and optionally data to long form XYZ in raster or trad matrix order. 

* All obvious raster functions now included (needs more testing). 

* Version bump to allow {grout} to depend on vaster for cell and extent ops. 

* Added a `NEWS.md` file to track changes to the package.
