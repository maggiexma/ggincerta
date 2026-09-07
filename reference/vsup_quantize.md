# Tree quantization for Value-Suppressing Uncertainty Palettes

Quantizes value and uncertainty variables into the hierarchical tree
structure used by Value-Suppressing Uncertainty Palettes (VSUPs).

## Usage

``` r
vsup_quantize(
  v,
  u,
  layers = 4,
  branch = 2L,
  breaks = list(NULL, NULL),
  limits = list(NULL, NULL),
  transform = list("identity", "identity")
)
```

## Arguments

- v:

  A numeric vector containing the value variable.

- u:

  A numeric vector containing the uncertainty variable.

- layers:

  An integer specifying the number of uncertainty levels.

- branch:

  An integer specifying the branching factor of the VSUP tree. The
  maximum number of value bins is `branch^(layers - 1)`.

- breaks:

  A list of two numeric vectors specifying break points for the value
  and uncertainty variables, respectively. If an element is `NULL`,
  breaks are generated automatically as equal-width intervals on the
  transformed scale. The value dimension requires
  `branch^(layers - 1) + 1` break points and the uncertainty dimension
  requires `layers + 1` break points.

- limits:

  A list of two numeric vectors specifying the ranges used for the value
  and uncertainty variables. If an element is `NULL`, the finite data
  range is used. Values outside these limits are not assigned to a VSUP
  leaf.

- transform:

  A list of one or two transformations applied to `v` and `u` before
  quantization. Each element can be a transformation name or a
  transformer object accepted by
  [`scales::as.transform()`](https://scales.r-lib.org/reference/new_transform.html).

## Value

A list with the following components:

- value:

  A factor containing the VSUP leaf identifier assigned to each
  observation. Observations outside the limits or with missing values
  are returned as `NA`.

- leaf_info:

  A data frame describing each VSUP leaf, including its leaf identifier,
  uncertainty layer, normalized value position, and value midpoint.

- value_breaks:

  The value break points on the original data scale.

- uncertainty_breaks:

  The uncertainty break points on the original data scale.

## Details

The uncertainty variable is divided into `layers` intervals. The number
of value bins varies across uncertainty levels according to `branch`,
with the maximum number of value bins equal to `branch^(layers - 1)`.
Higher uncertainty levels therefore use progressively fewer value bins.

When breaks are not supplied, both variables are divided using
equal-width intervals on their transformed scales. User-supplied breaks
are restricted to the corresponding limits and must provide the required
number of break points after limits are applied.
