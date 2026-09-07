# Colour fading palette

One or more supplied colours construct a value colour scale, then varies
perceptual properties such as lightness, saturation, or transparency
along the uncertainty dimension.

## Usage

``` r
bivar_fade_palette(
  colours,
  n_breaks,
  fade = c("lighten", "alpha", "desaturate"),
  alpha_range = c(1, 0.3),
  max_light = 0.7,
  max_desat = 0.9,
  space = "Lab"
)
```

## Arguments

- colours:

  A character vector of colours used as key points in interpolation.

- n_breaks:

  An integer or a length-two vector specifying the desired number of
  bins for each variable. The default is 4 for both variables. When
  `nice.breaks = TRUE`, this value is treated as a suggestion and the
  actual number of bins may differ. When `nice.breaks = FALSE`, exactly
  `n_breaks` equal-width bins are generated. Unequal desired numbers of
  bins are supported.

- fade:

  A character string specifying the fading method: `"lighten"`,
  `"desaturate"`, or `"alpha"`.

- alpha_range:

  A numeric vector of length two specifying the range of transparency
  values used when `fade = "alpha"`.

- max_light:

  A numeric value specifying the maximum amount of lightening applied
  across uncertainty levels.

- max_desat:

  A numeric value specifying the maximum amount of desaturation applied
  across uncertainty levels.

- space:

  A character string specifying the colour space used for interpolation.
