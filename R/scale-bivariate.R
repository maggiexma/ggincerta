#' @rdname scale_bivariate
#' @export
ScaleBivariate <- ggproto(
  "ScaleBivariate",
  ScaleDiscrete,
  drop = FALSE,
  na.value = NA,
  transform = function(self, x) {
    n_breaks <- self$n_breaks
    colors <- self$colors
    breaks <- self$breaks

    compute_bivariate <- function(x, y) {
      qx <- quantile(x, seq(0, 1, length.out = n_breaks[1] + 1), na.rm = TRUE)
      qy <- quantile(y, seq(0, 1, length.out = n_breaks[2] + 1), na.rm = TRUE)
      if (breaks == "equal" || length(unique(qx)) < n_breaks[1]) {
        qx <- seq(
          min(x, na.rm = TRUE),
          max(x, na.rm = TRUE),
          length.out = n_breaks[1] + 1
        )
      }
      if (breaks == "equal" || length(unique(qy)) < n_breaks[2]) {
        qy <- seq(
          min(y, na.rm = TRUE),
          max(y, na.rm = TRUE),
          length.out = n_breaks[2] + 1
        )
      }
      xb <- unique(as.numeric(qx))
      yb <- unique(as.numeric(qy))
      bin1 <- cut(x, breaks = xb, include.lowest = TRUE, labels = FALSE)
      bin2 <- cut(y, breaks = yb, include.lowest = TRUE, labels = FALSE)
      combo <- (bin2 - 1L) * n_breaks[1] + bin1
      list(value = factor(combo, levels = 1:prod(n_breaks)), xb = xb, yb = yb)
    }

    if (inherits(x, "bivariate")) {
      res <- compute_bivariate(
        sapply(x, function(x) x$v1),
        sapply(x, function(x) x$v2)
      )
    }
    n <- prod(n_breaks)
    cols <- self$palette(n)
    if (length(cols) < n) {
      cols <- rep_len(cols, n)
    }
    self$legend_cols <- unname(as.character(cols))

    self$guide <- guide_bivariate(
      key = setNames(list(self$legend_cols), self$aesthetics),
      value = as.character(seq_len(n)),
      label = as.character(seq_len(n)),
      n_breaks = n_breaks,
      label1 = format(res$xb, digits = 2),
      label2 = format(res$yb, digits = 2),
      title1 = self$name1 %||% attr(x, "vars")[1],
      title2 = self$name2 %||% attr(x, "vars")[2],
      size = self$guide_size,
      aesthetics = self$aesthetics
    )
    res$value
  },

  train_df = function(self, df, ...) {
    ggproto_parent(ScaleDiscrete, self)$train_df(df, ...)
  }
)

#' Bivariate color scales
#'
#' `scale_*_bivariate` creates a bivariate palette by mixing two colour ramps,
#' then implements the mapping by binning the variables `v1`, `v2` and assigning
#' each bin combination to a colour.
#'
#' @inheritParams ggplot2::discrete_scale
#' @param name1,name2 Optional names for `v1` and `v2`. Used as axis titles in
#'   the legend. If `NULL`, the default, the names are taken from the mapping.
#' @param colors A character vector of length two specifying the colors for the
#'   bivariate palette.
#' @param n_breaks An integer guiding the number of bins for each variable.
#' @param breaks Method used to bin the variables: `"quantile"` (the default)
#'   or `"equal"`.
#' @param flip Method used to flip the legend: `"none"` (the default),
#'   `"vertical"`, `"horizontal"`, or `"both"`.
#' @param guide_size A numeric value controlling the size of the legend graphic,
#'   in centimeters.
#' @param ... Other arguments passed to [ggplot2::discrete_scale()].
#'
#' @returns A `ScaleBivariate` ggproto object.
#'
#' @examples
#' # Create a bivariate fill scale
#' sc <- scale_fill_bivariate()
#' class(sc)
#' sc$palette(9)
#'
#' # Basic bivariate map
#' p <- ggplot(nc) + geom_sf(aes(fill = duo(value, sd)))
#'
#' # Customize axis labels
#' p + scale_fill_bivariate(name1 = 'var1', name2 = 'var2')
#'
#' @seealso [ggplot2::Scale] for the base `ggproto` class that all scale objects
#'   inherit from.
#'
#' @rdname scale_bivariate
#' @export
scale_fill_bivariate <- function(
  name1 = NULL,
  name2 = NULL,
  colors = c("gold", "red4"),
  n_breaks = 4,
  breaks = c("quantile", "equal"),
  flip = c("none", "vertical", "horizontal", "both"),
  guide_size = 1.5,
  na.value = NA,
  na.translate = TRUE,
  aesthetics = "fill",
  ...
) {
  flip <- match.arg(flip)
  breaks <- match.arg(breaks)

  pal_safe <- function(n) {
    bivar_palette(
      colors,
      n_breaks = rep(round(sqrt(n)), 2),
      flip = flip
    )
  }

  sc <- discrete_scale(
    aesthetics = aesthetics,
    palette = pal_safe,
    guide = "legend",
    drop = FALSE,
    na.value = na.value,
    na.translate = na.translate,
    super = ScaleBivariate,
    ...
  )
  if (length(n_breaks) == 1L && is.numeric(n_breaks)) {
    n_breaks <- c(n_breaks, n_breaks)
  }
  stopifnot(length(n_breaks) == 2)
  sc$n_breaks <- n_breaks
  sc$breaks <- breaks
  sc$colors <- colors
  sc$name1 <- name1
  sc$name2 <- name2
  sc$guide_size <- guide_size
  sc
}

#' @rdname scale_bivariate
#' @export
scale_color_bivariate <- function(
  name1 = NULL,
  name2 = NULL,
  colors = c("gold", "red4"),
  n_breaks = 4,
  breaks = c("quantile", "equal"),
  flip = c("none", "vertical", "horizontal", "both"),
  guide_size = 1.5,
  na.value = NA,
  na.translate = TRUE,
  aesthetics = "colour",
  ...
) {
  scale_fill_bivariate(
    name1 = name1,
    name2 = name2,
    colors = colors,
    n_breaks = n_breaks,
    breaks = breaks,
    flip = flip,
    guide_size = guide_size,
    na.value = na.value,
    na.translate = na.translate,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_bivariate
#' @export
scale_colour_bivariate <- scale_color_bivariate

#' @export
scale_type.bivariate <- function(x) "bivariate"



