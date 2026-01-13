GuideVSUP <- ggproto(
  "GuideVSUP",
  GuideLegend,

  params = c(
    GuideLegend$params,
    list(
      key = NULL,
      layer_sizes = NULL,
      value_breaks = NULL,
      uncertainty_breaks = NULL,
      title_value = NULL,
      title_uncertainty = NULL,
      aesthetics = NULL
    )
  ),

  extract_key = function(scale, aesthetic, key, ...) {
    key
  },

  draw = function(self, theme, params = self$params, ...) {

    draw_key_vsup(
      key = params$key,
      layer_sizes = params$layer_sizes,
      value_breaks = params$value_breaks,
      uncertainty_breaks = params$uncertainty_breaks,
      aesthetics = params$aesthetics %||% "fill",
      title_value = params$title_value,
      title_uncertainty = params$title_uncertainty
    )
  }
)

guide_vsup <- function(
    key,
    layer_sizes,
    value_breaks,
    uncertainty_breaks,
    title_value = "Value",
    title_uncertainty = "Uncertainty",
    ...,
    theme = NULL,
    title = waiver(),
    order = 0,
    position = NULL,
    aesthetics = NULL
) {

  key_df <- data.frame(key)

  new_guide(
    key = key_df,
    layer_sizes = layer_sizes,
    value_breaks = value_breaks,
    uncertainty_breaks = uncertainty_breaks,
    title_value = title_value,
    title_uncertainty = title_uncertainty,
    theme = theme,
    title = title,
    order = order,
    position = position,
    available_aes = c("fill", "colour"),
    aesthetics = aesthetics,
    super = GuideVSUP
  )
}
