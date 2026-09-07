# Create your own bivariate colour scale

These scales allow users to provide all colours used for the
combinations of two variables in a bivariate colour scale.

## Usage

``` r
manual_bivariate_scale(
  aesthetics,
  ...,
  values,
  name = waiver(),
  breaks = list(waiver(), waiver()),
  labels = list(waiver(), waiver()),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  na.value = NA,
  na.translate = TRUE,
  drop = FALSE,
  guide = guide_bivariate(),
  n_breaks = c(4, 4),
  quantile = FALSE,
  var1_name = NULL,
  var2_name = NULL,
  super = ScaleBivariate
)

scale_fill_bivariate_manual(
  ...,
  values,
  name = waiver(),
  var1_name = NULL,
  var2_name = NULL,
  n_breaks = c(4, 4),
  quantile = FALSE,
  breaks = list(waiver(), waiver()),
  labels = list(waiver(), waiver()),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  na.value = NA,
  na.translate = TRUE,
  aesthetics = "fill",
  guide = guide_bivariate()
)

scale_colour_bivariate_manual(
  ...,
  values,
  name = waiver(),
  var1_name = NULL,
  var2_name = NULL,
  n_breaks = c(4, 4),
  quantile = FALSE,
  breaks = list(waiver(), waiver()),
  labels = list(waiver(), waiver()),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  na.value = NA,
  na.translate = TRUE,
  aesthetics = "colour",
  guide = guide_bivariate()
)

scale_color_bivariate_manual(
  ...,
  values,
  name = waiver(),
  var1_name = NULL,
  var2_name = NULL,
  n_breaks = c(4, 4),
  quantile = FALSE,
  breaks = list(waiver(), waiver()),
  labels = list(waiver(), waiver()),
  limits = list(NULL, NULL),
  transform = list("identity", "identity"),
  na.value = NA,
  na.translate = TRUE,
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

- values:

  A character vector of colours. The length should be at least the
  product of `n_breaks`, giving one colour for each combination of the
  two binned variables.

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

- n_breaks:

  An integer or a length-two vector specifying the desired number of
  bins for each variable. The default is 4 for both variables. When
  `nice.breaks = TRUE`, this value is treated as a suggestion and the
  actual number of bins may differ. When `nice.breaks = FALSE`, exactly
  `n_breaks` equal-width bins are generated. Unequal desired numbers of
  bins are supported.

- quantile:

  A logical value or length-two logical vector indicating whether the
  variables should be divided using quantile-based bins. If quantile
  breaks are not unique, equal-width bins are used instead.

- var1_name, var2_name:

  Optional names for `v1` and `v2`. Used as axis titles in the legend.
  If `NULL`, the default, the names are taken from the mapping.

- super:

  The super class to use for the constructed scale

## Examples

``` r
ggplot(nc, aes(fill = duo(value, sd))) +
  geom_sf() +
  scale_fill_bivariate_manual(
    values = c(
      "#F7F4F9", "#D4B9DA", "#C994C7", "#980043",
      "#E0ECF4", "#BFD3E6", "#9EBCDA", "#8856A7",
      "#D0D1E6", "#A6BDDB", "#74A9CF", "#2B8CBE",
      "#B8E186", "#7FBC41", "#4D9221", "#276419"
    ),
    n_breaks = 4
  )
```
