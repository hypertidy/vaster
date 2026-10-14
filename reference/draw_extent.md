# Draw extent

Draw an extent with two clicks

## Usage

``` r
draw_extent(show = TRUE, ...)
```

## Arguments

- show:

  the drawn extent

- ...:

  arguments pass to
  [`graphics::rect()`](https://rdrr.io/r/graphics/rect.html)

## Value

an extent, numeric vector of xmin,xmax,ymin,ymax

## Examples

``` r
if (interactive()) {
plot(1)
draw_extent(show = TRUE)  ## click twice on the plot
}
```
