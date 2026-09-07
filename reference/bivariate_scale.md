# Bivariate colour scale constructor

`bivariate_scale()` maps binned combinations of two variables to colour
dimensions and their combinations in perceptual colour space, supporting
the construction of bivariate choropleth maps.

## Usage

``` r
ScaleBivariate

bivariate_scale(
  aesthetics,
  ...,
  name = waiver(),
  breaks = list(waiver(), waiver()),
  labels = list(waiver(), waiver()),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  na.value = NA,
  na.translate = TRUE,
  drop = FALSE,
  guide = waiver(),
  colours = c("#EDF8B1", "#2C7FB8"),
  palette_fun = NULL,
  palette_params = list(),
  n_breaks = c(4, 4),
  nice.breaks = TRUE,
  quantile = FALSE,
  var1_name = NULL,
  var2_name = NULL,
  super = ScaleBivariate
)

scale_fill_bivariate(
  ...,
  name = waiver(),
  var1_name = NULL,
  var2_name = NULL,
  colours = c("#EDF8B1", "#2C7FB8"),
  palette_fun = NULL,
  palette_params = list(),
  n_breaks = c(4, 4),
  nice.breaks = TRUE,
  quantile = FALSE,
  breaks = list(waiver(), waiver()),
  labels = list(waiver(), waiver()),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  na.value = NA,
  aesthetics = "fill",
  guide = guide_bivariate()
)

scale_color_bivariate(
  ...,
  name = waiver(),
  var1_name = NULL,
  var2_name = NULL,
  colours = c("#EDF8B1", "#2C7FB8"),
  palette_fun = NULL,
  palette_params = list(),
  n_breaks = c(4, 4),
  nice.breaks = TRUE,
  quantile = FALSE,
  breaks = list(waiver(), waiver()),
  labels = list(waiver(), waiver()),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  na.value = NA,
  aesthetics = "colour",
  guide = guide_bivariate()
)

scale_colour_bivariate(
  ...,
  name = waiver(),
  var1_name = NULL,
  var2_name = NULL,
  colours = c("#EDF8B1", "#2C7FB8"),
  palette_fun = NULL,
  palette_params = list(),
  n_breaks = c(4, 4),
  nice.breaks = TRUE,
  quantile = FALSE,
  breaks = list(waiver(), waiver()),
  labels = list(waiver(), waiver()),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  na.value = NA,
  aesthetics = "colour",
  guide = guide_bivariate()
)
```

## Arguments

- aesthetics:

  The names of the aesthetics that this scale works with.

- ...:

  Other arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

- name:

  The name of the scale. Used as the axis or legend title. If
  `waiver()`, the default, the name of the scale is taken from the first
  mapping used for that aesthetic. If `NULL`, the legend title will be
  omitted.

- breaks:

  A list of two numeric vectors specifying bin boundaries for the two
  variables. If `waiver()`, breaks are computed automatically according
  to `n_breaks`, `nice.breaks`, and `quantile`.

- labels:

  A list of two character vectors or labelling functions used to label
  the bin boundaries for the two variables. If `waiver()`, default
  labels are generated from the breaks.

- limits:

  A list of two vectors specifying the limits for the two variables. For
  continuous variables, each element specifies the numeric range to
  include. For discrete variables, it specifies the levels to include.

- transform:

  A list of one or two transformations applied to continuous variables
  before binning. Automatic pretty, equal-width, and quantile breaks are
  computed on the transformed scale. Each element can be a
  transformation name or a transformer object accepted by
  [`scales::as.transform()`](https://scales.r-lib.org/reference/new_transform.html).

- na.value:

  If `na.translate = TRUE`, what aesthetic value should the missing
  values be displayed as? Does not apply to position scales where `NA`
  is always placed at the far right.

- na.translate:

  Unlike continuous scales, discrete scales can easily show missing
  values, and do so by default. If you want to remove missing values
  from a discrete scale, specify `na.translate = FALSE`.

- drop:

  Should unused factor levels be omitted from the scale? The default,
  `TRUE`, uses the levels that appear in the data; `FALSE` includes the
  levels in the factor. Please note that to display every level in a
  legend, the layer should use `show.legend = TRUE`.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

- colours:

  A character vector of colours used as key points in the colour ramp
  that variables are mapped to. For details on how supplied colours are
  used to construct the resulting palette, see
  [`bivar_palette()`](https://maggiexma.github.io/ggincerta/reference/bivar_palette.md)
  and
  [`bivar_fade_palette()`](https://maggiexma.github.io/ggincerta/reference/bivar_fade_palette.md).

- palette_fun:

  A palette function that, when called with `colours` and `n_breaks`,
  returns a character vector of colours for all binned combinations. If
  `NULL`, the default,
  [`bivar_palette()`](https://maggiexma.github.io/ggincerta/reference/bivar_palette.md)
  is used.

- palette_params:

  A list of additional arguments passed to `palette_fun`. See
  [`bivar_palette()`](https://maggiexma.github.io/ggincerta/reference/bivar_palette.md)
  and
  [`bivar_fade_palette()`](https://maggiexma.github.io/ggincerta/reference/bivar_fade_palette.md)
  for available arguments.

- n_breaks:

  An integer or a length-two vector specifying the desired number of
  bins for each variable. The default is 4 for both variables. When
  `nice.breaks = TRUE`, this value is treated as a suggestion and the
  actual number of bins may differ. When `nice.breaks = FALSE`, exactly
  `n_breaks` equal-width bins are generated. Unequal desired numbers of
  bins are supported.

- nice.breaks:

  A logical value or length-two logical vector indicating whether
  automatically generated breaks should use pretty, human-readable
  values. When `TRUE`, breaks are generated using
  [`pretty()`](https://rdrr.io/r/base/pretty.html) on the transformed
  scale, with `n_breaks` treated as a suggestion. The resulting breaks
  may extend beyond the data range and the actual number of bins may
  differ from `n_breaks`. When `FALSE`, exactly `n_breaks` equal-width
  bins are generated.

- quantile:

  A logical value or length-two logical vector indicating whether the
  variables should be divided using quantile-based bins. If quantile
  breaks are not unique, equal-width bins are used instead.

- var1_name, var2_name:

  Optional names for `v1` and `v2`. Used as axis titles in the legend.
  If `NULL`, the default, the names are taken from the mapping.

- super:

  The super class to use for the constructed scale

## Value

A `ScaleBivariate` ggproto object.

## Details

It can be automatically dispatched in `aes()` using
[`duo()`](https://maggiexma.github.io/ggincerta/reference/duo.md) and
works with any ggplot2 geom.

## See also

[ggplot2::Scale](https://ggplot2.tidyverse.org/reference/Scale.html) for
the base ggproto class that all scale objects inherit from.

## Examples

``` r
# Basic bivariate map
ggplot(nc) +
  geom_sf(aes(fill = duo(value, sd)))


# Use an alternative bivariate palette
ggplot(nc) +
  geom_sf(aes(fill = duo(value, sd))) +
  scale_fill_bivariate(
    palette_fun = bivar_fade_palette,
    colours = c("#F6E8C3", "orange", "red")
  )


# Customize the desired number of bins
ggplot(nc) +
  geom_sf(aes(fill = duo(value, sd))) +
  scale_fill_bivariate(n_breaks = c(3, 4))


# Use exactly four equal-width bins for each variable
ggplot(nc) +
  geom_sf(aes(fill = duo(value, sd))) +
  scale_fill_bivariate(
    n_breaks = 4,
    nice.breaks = FALSE
  )
```
