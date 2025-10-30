#' #' Construct a distribution for exceedance probability
#' #'
#' #' Build a function `dist_fun(q, estimate, error)` that returns the exceedance
#' #' probability `P(X > q)` by combining a base R cumulative distribution
#' #' function `pfun` with a user-defined parameter transformation based on
#' #' `estimate` and `error`.
#' #'
#' #' @param pfun A CDF function. If `NULL` (the default), `stats::pnorm()` is used.
#' #' @param params A function that takes `estimate` and `error` as inputs
#' #'   and returns a list of parameters required by `pfun`.
#' #' @param lower.tail Logical. If `FALSE` (the default), the constructed
#' #'   function returns `P(X > q)`; if `TRUE`, it returns `P(X <= q)`.
#' #'
#' #' @examples
#' #' data(nc)
#' #' dist_exp <- exceed_dist(stats::pexp, function(estimate, error)
#' #'   list(rate = 1 / estimate))
#' #' ex_pr <- dist_exp(q = 3, estimate = nc$v1, error = nc$v2)
#' #'
#' #' @seealso [scale_fill_exceed()] for use in exceedance probability maps.
#' #' @export
#' exceed_dist <- function(pfun,
#'                         params = function(estimate, error)
#'                           list(),
#'                         lower.tail = FALSE) {
#'   browser()
#'   stopifnot(is.function(pfun), is.function(params))
#'   function(q, estimate, error) {
#'     browser()
#'     args <- c(list(q = q, lower.tail = lower.tail), params(estimate, error))
#'     do.call(pfun, args)
#'   }
#' }

#' @rdname scale_exceed
#' @export
ScaleExceed <- ggproto(
  "ScaleExceed",
  ScaleContinuous,

  transform = function(self, x) {
    browser()
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
#'   Typically, this should be the output of [exceed_dist()]. If `NULL`
#'   (the default), a normal distribution with `stats::pnorm()` is used.
#' @param threshold A numeric value specifying the threshold `q` in the exceedance
#'   probability expression `P(X > q)`.
#'
#' @examples
#' ggplot(nc) +
#'   geom_sf(aes(fill = duo_exceed(value, sd))) +
#'   scale_fill_exceed(threshold = 1.64)
#'
#' @rdname scale_exceed
#' @export
scale_fill_exceed <- function(name = NULL,
                              palette = "Oranges",
                              type = "seq",
                              direction = 1,
                              dist_fun = NULL,
                              threshold,
                              limits = c(0, 1),
                              na.value = NA,
                              guide = "colourbar",
                              aesthetics = "fill",
                              ...) {
  browser()
  if (missing(threshold))
    stop("`threshold` is required.")
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
                                threshold,
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
