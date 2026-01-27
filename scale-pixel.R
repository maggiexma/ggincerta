scale_type.pixel <- function(x) "pixel"

ScalePixel <- ggproto(
  "ScalePixel",
  ScaleContinuous,

  train = function(self, x) {
    if (inherits(self$name, "waiver")) {
      lab <- attr(x, "label", exact = TRUE)
      if (!is.null(lab))
        self$name <- lab
    }
    ggproto_parent(ScaleContinuous, self)$train(x)
  }
)

scale_fill_pixel <- function(type = "seq",
                             palette = "Oranges",
                             direction = 1,
                             name = waiver(),
                             ...) {

  pal <- scales::gradient_n_pal(
    scales::pal_brewer(
      type = type,
      palette = palette,
      direction = direction
    )(7)
  )

  continuous_scale(
    aesthetics = "fill",
    scale_name = "pixel",
    palette = pal,
    name = name,
    guide = guide_colourbar(),
    super = ScalePixel,
    ...
  )
}
