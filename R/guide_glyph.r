GuideGlyph <- ggproto(
  "GuideGlyph",
  GuideLegend,
  params = c(
    GuideLegend$params,
    list(
      breaks = NULL,
      glyph = NULL,
      label = NULL,
      font_size = NULL
    )
  ),
  draw = function(self, theme, params = self$params, ...) {
    draw_glyph_key(
      key = params$breaks,
      glyph = params$glyph,
      label = params$label,
      font_size = params$font_size
    )
  }
)

guide_glyph <- function(breaks,
                        glyph = NULL,
                        label = NULL,
                        font_size = NULL,
                        theme = NULL,
                        title = waiver(),
                        order = 99,
                        position = NULL) {
  new_guide(
    breaks = breaks,
    glyph = glyph,
    label = label,
    font_size = font_size,
    theme = theme,
    title = title,
    order = order,
    position = position,
    available_aes = "glyph",
    super = GuideGlyph
  )
}
