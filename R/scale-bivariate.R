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

    compute_bins <- function(x, n, method) {
      if (identical(method, "quantile")) {
        br <- quantile(
          x,
          probs = seq(0, 1, length.out = n + 1),
          na.rm = TRUE,
          names = FALSE
        )

        if (length(unique(br)) < (n + 1)) {
          br <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
        }
      } else {
        br <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
      }

      unique(as.numeric(br))
    }

    x1 <- vapply(x, function(z)
      z$v1, numeric(1))
    x2 <- vapply(x, function(z)
      z$v2, numeric(1))

    x_breaks <- compute_bins(x1, n_breaks[1], bin_method)
    y_breaks <- compute_bins(x2, n_breaks[2], bin_method)

    x_bin <- cut(x1,
                 breaks = x_breaks,
                 include.lowest = TRUE,
                 labels = FALSE)

    y_bin <- cut(x2,
                 breaks = y_breaks,
                 include.lowest = TRUE,
                 labels = FALSE)

    bin_id <- (y_bin - 1L) * n_breaks[1] + x_bin

    values <- factor(bin_id, levels = seq_len(prod(n_breaks)))

    key_colours <- self$palette(prod(n_breaks))
    if (length(key_colours) < prod(n_breaks)) {
      key_colours <- rep_len(key_colours, prod(n_breaks))
    }

    var_names <- attr(x, "vars")

    self$guide_info <- list(
      n_breaks = n_breaks,
      x_breaks = x_breaks,
      y_breaks = y_breaks,
      key_colours = unname(as.character(key_colours)),
      var1_title = self$var1_name %||% var_names[1],
      var2_title = self$var2_name %||% var_names[2],
      aesthetics = self$aesthetics,
      key_size = self$key_size
    )

    values
  },

  get_guide_info = function(self) {
    self$guide_info
  },

  train_df = function(self, df, ...) {
    ggproto_parent(ScaleDiscrete, self)$train_df(df, ...)
  }
)

bivariate_scale <- function(aesthetics,
                            palette,
                            ...,
                            name = waiver(),
                            breaks = waiver(),
                            labels = waiver(),
                            limits = NULL,
                            na.value = NA,
                            na.translate = TRUE,
                            drop = FALSE,
                            guide = waiver(),
                            n_breaks = c(4, 4),
                            bin_method = c("quantile", "equal"),
                            var1_name = NULL,
                            var2_name = NULL,
                            key_size = 1.5,
                            super = ScaleBivariate) {
  bin_method <- rlang::arg_match(bin_method)

  if (!is.function(palette)) {
    cli::cli_abort("{.arg palette} must be a function.")
  }

  sc <- discrete_scale(
    aesthetics = aesthetics,
    name = name,
    palette = palette,
    breaks = breaks,
    labels = labels,
    limits = limits,
    na.value = na.value,
    na.translate = na.translate,
    drop = drop,
    guide = guide,
    ...,
    super = super
  )

  sc$n_breaks <- n_breaks
  sc$bin_method <- bin_method
  sc$var1_name <- var1_name
  sc$var2_name <- var2_name
  sc$key_size <- key_size

  sc
}

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
scale_fill_bivariate <- function(var1_name = NULL,
                                 var2_name = NULL,
                                 colors = c("gold", "red4"),
                                 n_breaks = c(4, 4),
                                 bin_method = c("quantile", "equal"),
                                 flip = c("none", "vertical", "horizontal", "both"),
                                 key_size = 1.5,
                                 na.value = NA,
                                 na.translate = TRUE,
                                 aesthetics = "fill",
                                 guide = guide_bivariate(),
                                 ...) {
  bin_method <- match.arg(bin_method)
  flip <- match.arg(flip)

  if (length(n_breaks) == 1L) {
    n_breaks <- rep.int(as.integer(n_breaks), 2)
  } else {
    n_breaks <- as.integer(n_breaks)
  }

  pal_safe <- function(n) {
    bivar_palette(colors = colors,
                  n_breaks = n_breaks,
                  flip = flip)
  }

  sc <- bivariate_scale(
    aesthetics = aesthetics,
    palette = pal_safe,
    guide = guide,
    na.value = na.value,
    na.translate = na.translate,
    drop = FALSE,
    n_breaks = n_breaks,
    bin_method = bin_method,
    var1_name = var1_name,
    var2_name = var2_name,
    key_size = key_size,
    ...
  )

  sc$colors <- colors
  sc$flip <- flip
  sc
}

#' @rdname scale_bivariate
#' @export
scale_color_bivariate <- function(var1_name = NULL,
                                  var2_name = NULL,
                                  colors = c("gold", "red4"),
                                  n_breaks = c(4, 4),
                                  bin_method = c("quantile", "equal"),
                                  flip = c("none", "vertical", "horizontal", "both"),
                                  key_size = 1.5,
                                  na.value = NA,
                                  na.translate = TRUE,
                                  aesthetics = "colour",
                                  guide = guide_bivariate(),
                                  ...) {
  scale_fill_bivariate(
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
scale_type.bivariate <- function(x)
  "bivariate"
