# Value-Suppressing Uncertainty Palettes

`vsup_palette()` handles colour generation for VSUP scales. It modifies
the supplied base colours by progressively desaturating and lightening
them as uncertainty increases.

## Usage

``` r
vsup_palette(
  leaf_info,
  colours,
  layers = 4,
  branch = 2,
  max_light = 0.7,
  max_desat = 0.9,
  pow_light = 1,
  pow_desat = 1,
  space = "Lab"
)
```

## Arguments

- leaf_info:

  A data frame describing the VSUP bin structure, produced by
  [`vsup_quantize()`](https://maggiexma.github.io/ggincerta/reference/vsup_quantize.md).
  It stores the leaf identifiers, uncertainty layers, and value
  positions used to assign colours.

- colours:

  A character vector of colours used as key points for interpolating the
  value colour scale at the lowest uncertainty level.

- layers:

  An integer specifying the number of uncertainty levels in the VSUP
  hierarchy.

- branch:

  An integer specifying the branching factor used to determine the
  number of value bins across uncertainty levels. The maximum number of
  value bins is `branch^(layers - 1)`, with fewer value bins used at
  higher uncertainty levels.

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
