ScaleGlyph <- ggproto(
  "ScaleGlyph",
  ScaleContinuous,
  aesthetics = "glyph",

  map = function(self, x, limits = self$get_limits())
    x,

  train_df = function(self, df, ...) {
    ggproto_parent(ScaleContinuous, self)$train_df(df, ...)
    vals <- if ("glyph" %in% names(df))
      df$glyph
    else
      df$error
    vals <- vals[is.finite(vals)]
    if (!length(vals))
      vals <- 0

    brks <- as.numeric(stats::quantile(vals, probs = c(0, 0.5, 1), na.rm = TRUE))
    self$err_breaks <- brks

    self$guide <- guide_glyph(
      breaks = brks,
      glyph = self$style,
      label = self$name,
      font_size = self$font_size,
      order = self$order
    )
    invisible(self)
  }
)

scale_glyph_continuous <- function(name = waiver(),
                                   style = c("icone", "semi"),
                                   font_size = 8.8,
                                   order = 99) {

  style <- match.arg(style)
  ggproto(
    NULL,
    ScaleGlyph,
    name = name,
    style = style,
    font_size = font_size,
    order = order
  )
}
