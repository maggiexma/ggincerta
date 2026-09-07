# Bivariate colour guide

The bivariate colour guide is displayed as a grid, where each cell
represents the mapping between a colour and the corresponding bin
intervals of the two variables along the guide axes.

## Usage

``` r
GuideBivariate

guide_bivariate(
  theme = NULL,
  title = NULL,
  order = 0,
  position = NULL,
  rotated = FALSE,
  angle = 45
)
```

## Arguments

- theme:

  A [`theme`](https://ggplot2.tidyverse.org/reference/theme.html) object
  to style the guide individually or differently from the plot's theme
  settings. The `theme` argument in the guide partially overrides, and
  is combined with, the plot's theme. Arguments that apply to a single
  legend are respected, most of which have the `legend`-prefix.
  Arguments that apply to combined legends (the legend box) are ignored,
  including `legend.position`, `legend.justification.*`,
  `legend.location` and `legend.box.*`.

- title:

  A character string or expression indicating a title of guide. If
  `NULL`, the title is not shown. By default
  ([`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)),
  the name of the scale object or the name specified in
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) is used
  for the title.

- order:

  positive integer less than 99 that specifies the order of this guide
  among multiple guides. This controls the order in which multiple
  guides are displayed, not the contents of the guide itself. If 0
  (default), the order is determined by a secret algorithm.

- position:

  A character string indicating where the legend should be placed
  relative to the plot panels. One of "top", "right", "bottom", "left",
  or "inside".

- rotated:

  Logical indicating whether the guide should be displayed in a rotated
  orientation.

- angle:

  A numeric value specifying the rotation angle of the guide.
