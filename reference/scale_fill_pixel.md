# Pixel fill scale

Pixel fill scale

## Usage

``` r
scale_fill_pixel(
  type = "seq",
  palette = "Oranges",
  direction = 1,
  name = waiver(),
  ...
)
```

## Arguments

- type:

  One of "seq" (sequential), "div" (diverging) or "qual" (qualitative)

- palette:

  A palette function that when called with a numeric vector with values
  between 0 and 1 returns the corresponding output values (e.g.,
  [`scales::pal_area()`](https://scales.r-lib.org/reference/pal_area.html)).

- direction:

  Sets the order of colours in the scale. If 1, the default, colours are
  as output by
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html).
  If -1, the order of colours is reversed.

- name:

  The name of the scale. Used as the axis or legend title. If
  `waiver()`, the default, the name of the scale is taken from the first
  mapping used for that aesthetic. If `NULL`, the legend title will be
  omitted.

- ...:

  Additional arguments passed to
  [`ggplot2::continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html).
