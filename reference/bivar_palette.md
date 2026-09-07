# Colour blending palette

This palette function constructs two colour ramps from white to the
supplied endpoint colours, then blends them by additive averaging in
grDevices::rgb colour space. The resulting palette contains one colour
for each combination of the two binned variables.

## Usage

``` r
bivar_palette(
  colours = NULL,
  n_breaks = c(4, 4),
  flip = c("none", "vertical", "horizontal", "both")
)
```

## Arguments

- colours:

  A character vector of two colours used as the endpoints of the two
  colour ramps.

- n_breaks:

  An integer or a length-two vector specifying the desired number of
  bins for each variable. The default is 4 for both variables. When
  `nice.breaks = TRUE`, this value is treated as a suggestion and the
  actual number of bins may differ. When `nice.breaks = FALSE`, exactly
  `n_breaks` equal-width bins are generated. Unequal desired numbers of
  bins are supported.

- flip:

  A character string specifying how to flip the palette: `"none"` (the
  default), `"vertical"`, `"horizontal"`, or `"both"`.
