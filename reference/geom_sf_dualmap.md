# Dual map

`geom_sf_dualmap()` generates a map where each region is represented by
two visual components: the surrounding area is mapped using `fill`, and
the central glyph is mapped using `colour`.

## Usage

``` r
geom_sf_dualmap(
  mapping = NULL,
  data = NULL,
  ...,
  shape = "circle",
  max_angle = NULL,
  size = 1,
  point_fun = sf::st_point_on_surface,
  border_colour = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  angle_guide = TRUE,
  angle_name = waiver(),
  angle_order = 2,
  fill_scale = NULL
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

- shape:

  Glyph shape. One of `"circle"` (the default), `"square"`,
  `"triangle"`, `"hex"`, `"drop"`, or `"chernoff"`.

- max_angle:

  Maximum value of the `angle` aesthetic used for rescaling glyph
  rotation.

- size:

  A positive numeric scaling factor controlling glyph size.

- point_fun:

  Function used to calculate the representative point for each region.
  The default is usually
  [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- border_colour:

  Colour used for glyph borders.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

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

- angle_guide:

  Logical indicating whether to display a guide for the `angle`
  aesthetic.

- angle_name:

  Title used for the angle guide.

- angle_order:

  Order of the angle guide relative to other guides.

- fill_scale:

  Optional fill scale for the surrounding area. If `NULL`, a default
  grey gradient is added when the `fill` mapping is not created by
  [`duo()`](https://maggiexma.github.io/ggincerta/reference/duo.md).

## Value

A list of layers generated by
[`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) and
[`geom_sf_glyph()`](https://maggiexma.github.io/ggincerta/reference/geom_sf_glyph.md).

## Details

This allows two variables, such as value and uncertainty, to be shown
within the same geographic region using separate scales.

## Examples

``` r
# Dual map with separate fill and colour mappings
ggplot(nc) +
  geom_sf_dualmap(aes(fill = sd, colour = value))
#> Warning: st_point_on_surface assumes attributes are constant over geometries
#> Warning: st_point_on_surface may not give correct results for longitude/latitude data


# Dual map using a bivariate colour scale for the central glyph
ggplot(nc) +
  geom_sf_dualmap(aes(fill = duo(value, sd), colour = sd_log2))
#> Warning: st_point_on_surface assumes attributes are constant over geometries
#> Warning: st_point_on_surface may not give correct results for longitude/latitude data
```
