# Pixel map

`geom_sf_pixel()` generates a pixel map layer on areal sf data. Each
region is tessellated into small pixels, with pixel colours mapped from
values sampled from a specified distribution.

## Usage

``` r
StatPixel

geom_sf_pixel(
  mapping = NULL,
  data = NULL,
  n = 60,
  distribution = "uniform",
  seed = NULL,
  pixel_shape = "hex",
  flat_topped = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- n:

  integer of length 1 or 2, number of grid cells in x and y direction
  (columns, rows)

- distribution:

  Distribution used to sample pixel values within each region. Currently
  supports `"uniform"` (the default) and `"normal"`.

- seed:

  Integer seed used for reproducible sampling.

- pixel_shape:

  Shape of the generated pixels. One of `"hex"` (the default),
  `"square"`, or `"rect"`. `"rect"` is when dividing the x and y ranges
  into the same number of intervals, so cells may be rectangular.

- flat_topped:

  logical; if `TRUE` generate flat topped hexagons, else generate pointy
  topped

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes.

  You can also set this to one of "polygon", "line", and "point" to
  override the default legend.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

## Value

A list of ggplot2 layer objects.

## Details

Mappings in `geom_sf_pixel()` is also supplied with
[`duo_pixel()`](https://maggiexma.github.io/ggincerta/reference/duo.md)
inside `aes()`, which automatically dispatches an scale.

Since
[`sf::st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
is used internally, operating directly on geographic (s2) sf objects can
be slow, especially when a large number of pixels are generated.
Projecting data to a planar coordinate system in advance is recommended.

## Examples

``` r
# Transform sf data into a planar crs for faster geometric intersection
nc_flat <- sf::st_transform(nc, sf::st_crs(3857))

# Basic pixel map
ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
  geom_sf_pixel(n = 40)


# Control pixel shape and resolution
ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
  geom_sf_pixel(n = 30, pixel_shape = "square")
```
