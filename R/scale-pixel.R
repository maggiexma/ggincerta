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

#' Pixel fill scale
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams scales::pal_brewer
#' @param ... Additional arguments passed to [ggplot2::continuous_scale()].
#' @export
scale_fill_pixel <- function(type = "seq",
                             palette = "Oranges",
                             direction = 1,
                             name = waiver(),
                             ...) {
  pal <- scales::pal_gradient_n(
    scales::pal_brewer(
      type = type,
      palette = palette,
      direction = direction
    )(7)
  )

  continuous_scale(
    aesthetics = "fill",
    palette = pal,
    name = name,
    guide = guide_colourbar(),
    super = ScalePixel,
    ...
  )
}

#' @export
scale_type.pixel <- function(x) "pixel"
