# Changelog

## ggincerta 0.2.1

- Added `nc_sim`, a simulated areal dataset with linear trend, hotspot,
  and CAR spatial patterns at two signal strengths.

- `scale_bivariate()` now uses pretty breaks for automatic binning when
  `nice.breaks = TRUE`.

## ggincerta 0.2.0

CRAN release: 2026-05-25

### New features

- Added VSUP scales and guides for value-suppressing uncertainty
  palettes.

- Added regular glyph shapes for
  [`geom_sf_glyph()`](https://maggiexma.github.io/ggincerta/reference/geom_sf_glyph.md),
  including circle, square, triangle, hexagon.

- Added Chernoff face glyphs for glyph maps.

- Added
  [`geom_sf_dualmap()`](https://maggiexma.github.io/ggincerta/reference/geom_sf_dualmap.md)
  for combining a colour-filled choropleth layer with a centroid-based
  glyph layer.

### Improvements

- `scale_bivariate()` now supports user-specified `breaks`, `limits`,
  `labels`, and transformations.

- `scale_bivariate()` now supports unequal numbers of groups across the
  two mapped variables.

- Pixel maps now support different pixel shapes.

### Deprecated and removed

- Removed exceedance probability maps.

## ggincerta 0.1.0

CRAN release: 2025-11-11

- Initial CRAN submission.
