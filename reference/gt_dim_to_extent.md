# Determine extent from geotransform vector and dimension

Create the extent (xlim, ylim) from the geotransform and dimensions of
the grid.

## Usage

``` r
gt_dim_to_extent(x, dim)
```

## Arguments

- x:

  geotransform parameters, as per
  [`geo_transform0()`](https://hypertidy.github.io/vaster/reference/geo_transform0.md)

- dim:

  dimensions x,y of grid (ncol,nrow)

## Value

4-element extent c(xmin,xmax,ymin,ymax)

## Details

The extent is `c(xmin, xmax, ymin, ymax)`.

## Examples

``` r
gt_dim_to_extent(geo_transform0(c(1, -1), c(0, 10)), c(5, 10))
#> xmin xmin ymax ymax 
#>    0    5    0   10 
```
