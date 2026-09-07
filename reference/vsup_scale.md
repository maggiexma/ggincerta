# Value-Suppressing Uncertainty Palette (VSUP) scale

This scale implements Value-Suppressing Uncertainty Palettes (VSUPs),
proposed by Correll et al. (2018). VSUPs reduce colour variation as
uncertainty increases, so that value differences are progressively
suppressed in less reliable regions.

## Usage

``` r
ScaleVSUP

scale_fill_vsup(
  name = waiver(),
  colours = RColorBrewer::brewer.pal(3, "YlGnBu"),
  layers = 4,
  branch = 2L,
  breaks = list(NULL, NULL),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  title_value = "Value",
  title_uncertainty = "Uncertainty",
  na.value = NA,
  na.translate = TRUE,
  aesthetics = "fill",
  max_light = 0.7,
  max_desat = 0.9,
  pow_light = 1,
  pow_desat = 1,
  space = "Lab",
  guide = guide_vsup(),
  ...
)

scale_colour_vsup(
  name = waiver(),
  colours = RColorBrewer::brewer.pal(3, "YlGnBu"),
  layers = 4,
  branch = 2L,
  breaks = list(NULL, NULL),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  title_value = "Value",
  title_uncertainty = "Uncertainty",
  na.value = NA,
  na.translate = TRUE,
  aesthetics = "colour",
  max_light = 0.7,
  max_desat = 0.9,
  pow_light = 1,
  pow_desat = 1,
  space = "Lab",
  guide = guide_vsup(),
  ...
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  `waiver()`, the default, the name of the scale is taken from the first
  mapping used for that aesthetic. If `NULL`, the legend title will be
  omitted.

- colours:

  A character vector of colours used as key points in the value colour
  scale. See
  [`vsup_palette()`](https://maggiexma.github.io/ggincerta/reference/vsup_palette.md)
  for details.

- layers:

  An integer specifying the number of uncertainty levels in the VSUP
  hierarchy.

- branch:

  An integer specifying the branching factor used to determine the
  number of value bins across uncertainty levels. The maximum number of
  value bins is `branch^(layers - 1)`, with fewer value bins used at
  higher uncertainty levels.

- breaks:

  A list of two numeric vectors specifying break points for the value
  and uncertainty variables, respectively. If an element is `NULL`,
  breaks are generated automatically as equal-width intervals on the
  transformed scale. The value dimension uses `branch^(layers - 1) + 1`
  break points, while the uncertainty dimension uses `layers + 1` break
  points.

- limits:

  A list of two numeric vectors specifying the ranges used for the value
  and uncertainty variables. If an element is `NULL`, the finite range
  of the corresponding variable is used. Values outside the specified
  limits are mapped to `na.value`.

- transform:

  A list of one or two transformations applied to the value and
  uncertainty variables before quantization. Automatic breaks are
  generated as equal-width intervals on the transformed scale. Each
  element can be a transformation name or a transformer object accepted
  by
  [`scales::as.transform()`](https://scales.r-lib.org/reference/new_transform.html).

- title_value, title_uncertainty:

  Optional titles for the value and uncertainty dimensions in the guide.

- na.value:

  If `na.translate = TRUE`, what aesthetic value should the missing
  values be displayed as? Does not apply to position scales where `NA`
  is always placed at the far right.

- na.translate:

  Unlike continuous scales, discrete scales can easily show missing
  values, and do so by default. If you want to remove missing values
  from a discrete scale, specify `na.translate = FALSE`.

- aesthetics:

  The names of the aesthetics that this scale works with.

- max_light:

  A numeric value specifying the maximum amount of lightening applied
  across uncertainty levels.

- max_desat:

  A numeric value specifying the maximum amount of desaturation applied
  across uncertainty levels.

- pow_light, pow_desat:

  Numeric values controlling the rate of lightening and desaturation
  across uncertainty levels.

- space:

  A character string specifying the colour space used for colour
  interpolation.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

- ...:

  Other arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

## See also

Correll et al. (2018)
[doi:10.1145/3173574.3174216](https://doi.org/10.1145/3173574.3174216)
for technical details.
