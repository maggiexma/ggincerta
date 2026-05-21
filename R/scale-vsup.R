#' @rdname vsup_scale
#' @export
ScaleVSUP <- ggproto(
  "ScaleVSUP",
  ScaleDiscrete,

  drop = FALSE,
  na.value = NA,

  transform = function(self, x) {
    if (is.null(x)) {
      return(NULL)
    }

    v <- vapply(x, function(xx)
      xx$v1, numeric(1))
    u <- vapply(x, function(xx)
      xx$v2, numeric(1))

    res <- vsup_quantize(
      v = v,
      u = u,
      layers = self$layers,
      branch = self$branch %||% 2L,
      breaks = self$breaks %||% list(NULL, NULL),
      limits = self$limits %||% list(NULL, NULL),
      transform = self$transform_spec %||% list("identity", "identity")
    )

    leaf_info <- res$leaf_info
    if (is.null(leaf_info)) {
      return(res$value)
    }

    leaf_info <- leaf_info[order(leaf_info$leaf), , drop = FALSE]
    layer_sizes <- as.integer(table(leaf_info$layer))

    legend_cols <- vsup_palette(
      leaf_info = leaf_info,
      colours = self$colours,
      branch = self$branch,
      layers = self$layers,
      max_light = self$max_light,
      max_desat = self$max_desat,
      pow_light = self$pow_light,
      pow_desat = self$pow_desat,
      space = self$space
    )

    self$guide_info <- list(
      key_colours = unname(as.character(legend_cols)),
      layer_sizes = layer_sizes,
      value_breaks = res$value_breaks,
      uncertainty_breaks = res$uncertainty_breaks,
      title_value = self$title_value,
      title_uncertainty = self$title_uncertainty,
      aesthetics = self$aesthetics
    )

    res$value
  },

  map = function(self, x, limits = self$get_limits()) {
    if (is.null(x)) {
      return(NULL)
    }

    key <- suppressWarnings(as.integer(as.character(x)))
    cols <- self$guide_info$key_colours

    if (is.null(cols)) {
      return(rep(self$na.value %||% NA_character_, length(x)))
    }

    out <- cols[key]
    out[is.na(key)] <- self$na.value %||% NA_character_
    out
  },

  get_guide_info = function(self) {
    self$guide_info
  },

  train = function(self, x)
    invisible(),

  train_df = function(self, df, ...)
    invisible()
)

#' Value-Suppressing Uncertainty Palette (VSUP) scale
#'
#' This scale implements Value-Suppressing Uncertainty Palettes (VSUPs),
#' proposed by Correll et al. (2018). The main idea is to suppress colour
#' variation in regions with higher uncertainty, thereby directing visual
#' attention towards more reliable value differences.
#'
#' @seealso Correll et al. (2018) \doi{10.1145/3173574.3174216} for technical
#'   details.
#'
#' @inheritParams ggplot2::discrete_scale
#' @inheritParams bivariate_scale
#' @param colours A character vector of colours used as key points in the value
#'   colour scale. See [vsup_palette()] for details.
#' @param layers An integer specifying the number of uncertainty levels.
#' @param branch An integer specifying the branching factor used to allocate
#'   value bins across uncertainty levels. The maximum number of value bins is
#'   `branch^(layers - 1)`, and higher uncertainty levels are assigned fewer
#'   value bins.
#' @param title_value,title_uncertainty Optional titles for the value and
#'   uncertainty dimensions in the guide.
#' @param max_light A numeric value specifying the maximum amount of lightening
#'   applied across uncertainty levels.
#' @param max_desat A numeric value specifying the maximum amount of
#'   desaturation applied across uncertainty levels.
#' @param pow_light,pow_desat Numeric values controlling the rate of lightening
#'   and desaturation across uncertainty levels.
#' @param space A character string specifying the colour space used for colour
#'   interpolation.
#' @rdname vsup_scale
#' @export
scale_fill_vsup <- function(name = waiver(),
                            colours = c("gold", "red4"),
                            layers = 4,
                            branch = 2L,
                            breaks = list(NULL, NULL),
                            limits = list(NULL, NULL),
                            transform = list("identity", "identity"),
                            title_value = "Value",
                            title_uncertainty = "Uncertainty",
                            na.value = NA,
                            na.translate = TRUE,
                            aesthetics = "fill",
                            max_light = 0.7,
                            max_desat = 0.9,
                            pow_light = 1,
                            pow_desat = 1,
                            space = "Lab",
                            guide = guide_vsup(),
                            ...) {
  if (!is.list(breaks))
    breaks <- list(breaks)
  if (length(breaks) == 1)
    breaks <- rep(breaks, 2)

  if (!is.list(limits))
    limits <- list(limits)
  if (length(limits) == 1)
    limits <- rep(limits, 2)

  if (!is.list(transform))
    transform <- list(transform)
  if (length(transform) == 1)
    transform <- rep(transform, 2)

  sc <- discrete_scale(
    name = name,
    aesthetics = aesthetics,
    palette = function(n)
      rep(NA_character_, n),
    guide = guide,
    drop = FALSE,
    na.value = na.value,
    na.translate = na.translate,
    super = ScaleVSUP,
    ...
  )

  sc$layers <- as.integer(layers)
  sc$branch <- as.integer(branch)
  sc$colours <- colours
  sc$breaks <- breaks
  sc$limits <- limits
  sc$transform_spec <- transform
  sc$max_light <- max_light
  sc$max_desat <- max_desat
  sc$pow_light <- pow_light
  sc$pow_desat <- pow_desat
  sc$space <- space
  sc$title_value <- title_value
  sc$title_uncertainty <- title_uncertainty

  sc
}

#' @rdname vsup_scale
#' @export
scale_colour_vsup <- function(name = waiver(),
                              colours = c("gold", "red4"),
                              layers = 4,
                              branch = 2L,
                              breaks = list(NULL, NULL),
                              limits = list(NULL, NULL),
                              transform = list("identity", "identity"),
                              title_value = "Value",
                              title_uncertainty = "Uncertainty",
                              na.value = NA,
                              na.translate = TRUE,
                              aesthetics = "colour",
                              max_light = 0.7,
                              max_desat = 0.9,
                              pow_light = 1,
                              pow_desat = 1,
                              space = "Lab",
                              guide = guide_vsup(),
                              ...) {
  scale_fill_vsup(
    name = name,
    colours = colours,
    layers = layers,
    branch = branch,
    breaks = breaks,
    limits = limits,
    transform = transform,
    title_value = title_value,
    title_uncertainty = title_uncertainty,
    na.value = na.value,
    na.translate = na.translate,
    aesthetics = aesthetics,
    max_light = max_light,
    max_desat = max_desat,
    pow_light = pow_light,
    pow_desat = pow_desat,
    space = space,
    guide = guide,
    ...
  )
}
