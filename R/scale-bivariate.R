#' @rdname scale_bivariate
#' @export
ScaleBivariate <- ggproto(
  "ScaleBivariate",
  ScaleDiscrete,

  drop = FALSE,
  na.value = NA,

  transform = function(self, x) {
    if (!inherits(x, "bivariate")) {
      return(x)
    }

    n_breaks <- self$n_breaks
    bin_method <- self$bin_method

    compute_bins <- function(x1, x2) {
      x_breaks <- quantile(x1,
                           probs = seq(0, 1, length.out = n_breaks[1] + 1),
                           na.rm = TRUE)
      y_breaks <- quantile(x2,
                           probs = seq(0, 1, length.out = n_breaks[2] + 1),
                           na.rm = TRUE)

      if (bin_method == "equal" ||
          length(unique(x_breaks)) < (n_breaks[1] + 1)) {
        x_breaks <- seq(min(x1, na.rm = TRUE), max(x1, na.rm = TRUE), length.out = n_breaks[1] + 1)
      }

      if (bin_method == "equal" ||
          length(unique(y_breaks)) < (n_breaks[2] + 1)) {
        y_breaks <- seq(min(x2, na.rm = TRUE), max(x2, na.rm = TRUE), length.out = n_breaks[2] + 1)
      }

      x_breaks <- unique(as.numeric(x_breaks))
      y_breaks <- unique(as.numeric(y_breaks))

      x_bin <- cut(x1,
                   breaks = x_breaks,
                   include.lowest = TRUE,
                   labels = FALSE)
      y_bin <- cut(x2,
                   breaks = y_breaks,
                   include.lowest = TRUE,
                   labels = FALSE)

      bin_id <- (y_bin - 1L) * n_breaks[1] + x_bin

      list(
        values = factor(bin_id, levels = seq_len(prod(n_breaks))),
        x_breaks = x_breaks,
        y_breaks = y_breaks
      )
    }

    x1 <- vapply(x, function(z)
      z$v1, numeric(1))
    x2 <- vapply(x, function(z)
      z$v2, numeric(1))

    binned <- compute_bins(x1, x2)

    n <- prod(n_breaks)
    key_colours <- self$palette(n)
    if (length(key_colours) < n) {
      key_colours <- rep_len(key_colours, n)
    }

    var_names <- attr(x, "vars")

    self$guide_info <- list(
      n_breaks = n_breaks,
      x_breaks = binned$x_breaks,
      y_breaks = binned$y_breaks,
      key_colours = unname(as.character(key_colours)),
      var1_title = self$var1_name %||% var_names[1],
      var2_title = self$var2_name %||% var_names[2],
      aesthetics = self$aesthetics,
      key_size = self$key_size
    )

    binned$values
  },

  get_guide_info = function(self) {
    self$guide_info
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
    type = c("bivariate", "vsup"),
    var1_name = NULL,
    var2_name = NULL,
    colors = c("gold", "red4"),
    n_breaks = 4,
    bin_method = c("quantile", "equal"),
    flip = c("none", "vertical", "horizontal", "both"),
    key_size = 1.5,
    values = NULL,
    layers = 4,
    branch = 2L,
    title_value = "Value",
    title_uncertainty = "Uncertainty",
    max_light = 0.5,
    max_desat = 0.9,
    pow_light = 1,
    pow_desat = 1,
    na.value = NA,
    na.translate = TRUE,
    aesthetics = "fill",
    guide = guide_bivariate(),
    ...
) {
  type <- match.arg(type)

  if (type == "vsup") {
    return(scale_fill_vsup(
      values = values,
      layers = layers,
      branch = branch,
      title_value = title_value,
      title_uncertainty = title_uncertainty,
      na.value = na.value,
      na.translate = na.translate,
      aesthetics = aesthetics,
      max_light = max_light,
      max_desat = max_desat,
      pow_light = pow_light,
      pow_desat = pow_desat,
      ...
    ))
  }

  flip <- match.arg(flip)
  bin_method <- match.arg(bin_method)

  if (length(n_breaks) == 1L && is.numeric(n_breaks)) {
    n_breaks <- c(n_breaks, n_breaks)
  }
  stopifnot(length(n_breaks) == 2)

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
    guide = guide,
    drop = FALSE,
    na.value = na.value,
    na.translate = na.translate,
    super = ScaleBivariate,
    ...
  )

  sc$n_breaks <- n_breaks
  sc$bin_method <- bin_method
  sc$colors <- colors
  sc$var1_name <- var1_name
  sc$var2_name <- var2_name
  sc$key_size <- key_size
  sc
}

#' @rdname scale_bivariate
#' @export
scale_color_bivariate <- function(
    var1_name = NULL,
    var2_name = NULL,
    colors = c("gold", "red4"),
    n_breaks = 4,
    bin_method = c("quantile", "equal"),
    flip = c("none", "vertical", "horizontal", "both"),
    key_size = 1.5,
    na.value = NA,
    na.translate = TRUE,
    aesthetics = "colour",
    guide = guide_bivariate(),
    ...
) {
  scale_fill_bivariate(
    type = "bivariate",
    var1_name = var1_name,
    var2_name = var2_name,
    colors = colors,
    n_breaks = n_breaks,
    bin_method = bin_method,
    flip = flip,
    key_size = key_size,
    na.value = na.value,
    na.translate = na.translate,
    aesthetics = aesthetics,
    guide = guide,
    ...
  )
}

#' @rdname scale_bivariate
#' @export
scale_colour_bivariate <- scale_color_bivariate

#' @export
scale_type.bivariate <- function(x) "bivariate"



