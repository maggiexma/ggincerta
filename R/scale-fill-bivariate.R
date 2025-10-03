#' @export
ScaleFillBivariate <- ggproto(
  "ScaleFillBivariate",
  ScaleDiscrete,
  aesthetics = "fill",
  drop = FALSE,
  na.value = NA,

  train_df = function(self, df, ...) {
    if ("fill" %in% names(df)) {
      info <- attr(df$fill, "bivar_breaks", exact = TRUE)
      if (!is.null(info)) {
        self$prm_breaks <- info$prm_breaks
        self$scd_breaks <- info$scd_breaks
        self$n_prm <- info$n_prm %||% 3L
        self$n_scd <- info$n_scd %||% 3L
      }
    }

    ggproto_parent(ScaleDiscrete, self)$train_df(df, ...)

    n <- as.integer(self$n_prm * self$n_scd)
    cols <- self$palette(n)
    if (length(cols) < n)
      cols <- rep_len(cols, n)
    self$legend_cols <- unname(as.character(cols))

    self$guide <- guide_bivariate(
      aesthetic = setNames(list(self$legend_cols), "fill"),
      value = as.character(seq_len(n)),
      label = as.character(seq_len(n)),
      n_breaks = c(self$n_prm, self$n_scd),
      prm_label = format(self$prm_breaks, digits = 2),
      scd_label = format(self$scd_breaks, digits = 2),
      prm_text = self$prm_text %||% "primary",
      scd_text = self$scd_text %||% "secondary",
      size = self$guide_size
    )

    invisible()
  }
)

scale_fill_bivariate <- function(name = waiver(),
                                 name_primary = waiver(),
                                 name_secondary = waiver(),
                                 colors = c("gold", "red4"),
                                 n_breaks = waiver(),
                                 blend = c("additive", "subtractive"),
                                 flip = c("none", "vertical", "horizontal", "both"),
                                 guide_size = 1.5,
                                 na.value = NA,
                                 na.translate = TRUE,
                                 ...) {
  flip <- match.arg(flip)
  blend <- match.arg(blend)

  pal_safe <- function(n) {
    bivar_palette(colors[1],
                  colors[2],
                  n_breaks = rep(round(sqrt(n)), 2),
                  blend = blend,
                  flip = flip)
  }

  sc <- discrete_scale(
    aesthetics = "fill",
    palette = pal_safe,
    name = name,
    guide = "legend",
    drop = FALSE,
    na.value = na.value,
    na.translate = na.translate,
    super = ScaleFillBivariate,
    ...
  )
  sc$prm_text <- name_primary
  sc$scd_text <- name_secondary
  sc$guide_size <- guide_size
  sc
}
