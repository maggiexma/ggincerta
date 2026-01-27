ScaleVSUP <- ggproto(
  "ScaleVSUP",
  ScaleDiscrete,

  drop = FALSE,
  na.value = NA,

  transform = function(self, x) {
    layers <- self$layers
    branch <- self$branch %||% 2L

    v <- vapply(x, function(xx)
      xx$v1, numeric(1))
    u <- vapply(x, function(xx)
      xx$v2, numeric(1))

    res <- vsup_quantize(v, u, layers = layers, branch = branch)

    leaf_info <- res$leaf_info
    if (is.null(leaf_info))
      return(res$value)

    leaf_info <- leaf_info[order(leaf_info$leaf), , drop = FALSE]
    self$leaf_info <- leaf_info
    self$layer_sizes <- as.integer(table(leaf_info$layer))

    pal <- pal_vsup(
      values = self$values,
      unc_levels = layers,
      max_light = self$max_light,
      max_desat = self$max_desat,
      pow_light = self$pow_light,
      pow_desat = self$pow_desat
    )

    self$legend_cols <- unname(as.character(pal(leaf_info$v, leaf_info$u)))

    value_breaks <- seq(min(v, na.rm = TRUE), max(v, na.rm = TRUE), length.out = 2 * layers + 1)
    uncertainty_breaks <- seq(min(u, na.rm = TRUE), max(u, na.rm = TRUE), length.out = layers + 1)

    self$guide <- guide_vsup(
      key = setNames(list(self$legend_cols), self$aesthetics),
      layer_sizes = self$layer_sizes,
      value_breaks = format(value_breaks, digits = 1),
      uncertainty_breaks = format(uncertainty_breaks, digits = 1),
      title_value = self$title_value,
      title_uncertainty = self$title_uncertainty,
      aesthetics = self$aesthetics
    )

    res$value
  },

  map = function(self, x, limits = self$get_limits()) {
    if (is.null(x))
      return(NULL)

    key <- suppressWarnings(as.integer(as.character(x)))

    cols <- self$legend_cols
    if (is.null(cols)) {
      return(rep(self$na.value %||% NA_character_, length(x)))
    }

    out <- cols[key]
    out[is.na(key)] <- self$na.value %||% NA_character_
    out
  },

  train_df = function(self, df, ...) {
    ggproto_parent(ScaleDiscrete, self)$train_df(df, ...)
  }
)

scale_fill_vsup <- function(values = NULL,
                            layers = 4,
                            branch = 2L,
                            title_value = "Value",
                            title_uncertainty = "Uncertainty",
                            na.value = NA,
                            na.translate = TRUE,
                            aesthetics = "fill",
                            max_light = 0.9,
                            max_desat = 0.7,
                            pow_light = 0.8,
                            pow_desat = 1,
                            ...) {
  if (is.null(values)) {
    values <- bivar_palette(colors = c("gold", "red4"),
                            n_breaks = c(3, 3))[seq_len(8)]
  }
  sc <- discrete_scale(
    aesthetics = aesthetics,
    scale_name = "vsup",
    palette = function(n)
      rep(NA_character_, n),
    guide = "legend",
    drop = FALSE,
    na.value = na.value,
    na.translate = na.translate,
    super = ScaleVSUP,
    ...
  )

  sc$layers <- layers
  sc$branch <- branch

  sc$values <- values
  sc$max_light <- max_light
  sc$max_desat <- max_desat
  sc$pow_light <- pow_light
  sc$pow_desat <- pow_desat

  sc$title_value <- title_value
  sc$title_uncertainty <- title_uncertainty

  sc
}
