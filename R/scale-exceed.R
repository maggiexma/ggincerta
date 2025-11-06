#' @rdname scale_exceed
#' @export
ScaleExceed <- ggproto(
  "ScaleExceed",
  ScaleContinuous,

  transform = function(self, x) {
    v1 <- vapply(x, function(e)
      e$v1, numeric(1))
    v2 <- vapply(x, function(e)
      e$v2, numeric(1))
    p <- self$dist_fun(self$threshold, v1, v2)
    p
  }
)

#' Exceedance probability colour scales
#'
#' `scale_*_exceed` computes exceedance probabilities from a specified
#' distribution and maps onto a continuous gradient colour scale.
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams scales::pal_brewer
#' @param dist_fun A function used to compute the exceedance probability.
#'   If `NULL` (the default), a normal distribution with `stats::pnorm()` is used.
#' @param threshold A numeric value specifying the threshold `q` in the exceedance
#'   probability expression `P(X > q)`.
#' @param ... Other arguments passed to [ggplot2::continuous_scale()].
#'
#' @returns A `ScaleExceed` ggproto object.
#'
#' @examples
#' # Create an exceedance probability scale
#' sc <- scale_fill_exceed()
#' class(sc)
#'
#' # Basic bivariate map
#' p <- ggplot(nc) + geom_sf(aes(fill = duo_exceed(value, sd)))
#'
#' @rdname scale_exceed
#' @export
scale_fill_exceed <- function(name = NULL,
                              palette = "Oranges",
                              type = "seq",
                              direction = 1,
                              dist_fun = NULL,
                              threshold = 1.64,
                              limits = c(0, 1),
                              na.value = NA,
                              guide = "colourbar",
                              aesthetics = "fill",
                              ...) {
  if (is.null(dist_fun))
    dist_fun <- dist_norm

  pal <- scales::pal_gradient_n(
    colours = scales::pal_brewer(
      type = type,
      palette = palette,
      direction = direction
    )(7),
    values = NULL,
    space = "Lab"
  )
  if (is.null(name))
    name <- bquote(P(X > .(threshold)))

  sc <- continuous_scale(
    aesthetics = aesthetics,
    palette = pal,
    limits = limits,
    na.value = na.value,
    guide = guide,
    super = ScaleExceed,
    ...
  )
  sc$dist_fun <- dist_fun
  sc$threshold <- threshold
  sc$name <- name
  sc
}

dist_norm <- function(x, v1, v2) {
  stats::pnorm(x,
               mean = v1,
               sd = v2,
               lower.tail = FALSE)
}

#' @export
scale_type.exceed <- function(x) "exceed"

#' @rdname scale_exceed
#' @export
scale_colour_exceed <- function(name = NULL,
                                palette = "Oranges",
                                type = "seq",
                                direction = 1,
                                dist_fun = NULL,
                                threshold = 1.64,
                                limits = c(0, 1),
                                na.value = NA,
                                guide = "colourbar",
                                aesthetics = "colour",
                                ...) {
  scale_fill_exceed(
    name = name,
    palette = palette,
    type = type,
    direction = direction,
    dist_fun = dist_fun,
    threshold = threshold,
    limits = limits,
    na.value = na.value,
    guide = guide,
    aesthetics = aesthetics,
    ...
  )
}
