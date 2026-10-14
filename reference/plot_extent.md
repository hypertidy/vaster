# Plot an extent

Plot an extent

## Usage

``` r
plot_extent(x, ..., asp = 1, add = FALSE, border = "black")
```

## Arguments

- x:

  extent xmin,xmax,ymin,ymax

- ...:

  arguments passed to
  [`graphics::rect()`](https://rdrr.io/r/graphics/rect.html)

- asp:

  aspect ratio (1 by default)

- add:

  add to plot or initiated one, `FALSE` by default

- border:

  colour of lines of extent

## Value

nothing, used for plot side effect

## Examples

``` r
plot_extent(c(-180, 180, -90, 90))
plot_extent(c(100, 150, -60, -30), add = TRUE, border = "firebrick")
```
