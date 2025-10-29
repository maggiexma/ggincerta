exceed_dist <- function(pfun, params = function(v1, v2) list(), lower.tail = FALSE) {
  browser()
  stopifnot(is.function(pfun), is.function(params))
  function(q, v1, v2) {
    args <- c(list(q = q, lower.tail = lower.tail), params(v1, v2))
    do.call(pfun, args)
  }
}


ScaleExceed <- ggproto(
  "ScaleExceed",
  ScaleContinuous,

  transform = function(self, x) {
    browser()
    v1 <- vapply(x, function(e) e$v1, numeric(1))
    v2 <- vapply(x, function(e) e$v2, numeric(1))
    p  <- self$dist_fun(q = self$threshold, v1 = v1, v2 = v2)
    p
  }
)

scale_fill_exceed <- function(
    name = NULL,
    palette = "Oranges",
    dist_fun = NULL,
    threshold,
    limits = c(0, 1),
    na.value = NA,
    guide = "colourbar",
    ...
) {
  browser()
  if (missing(threshold)) stop("`threshold` is required.")
  if (is.null(dist_fun))  dist_fun <- dist_norm

  pal <- scales::gradient_n_pal(RColorBrewer::brewer.pal(9, palette))
  if (is.null(name)) name <- bquote(P(X > .(threshold)))

  sc <- continuous_scale(
    aesthetics = "fill",
    name = "exceed",
    palette = pal,
    limits = limits,
    na.value = na.value,
    guide = guide,
    super = ScaleExceed,
    ...
  )
  sc$dist_fun  <- dist_fun
  sc$threshold <- threshold
  sc$name      <- name
  sc
}


#' @export
dist_norm <- function(x, v1, v2) {
  stats::pnorm(x, mean = v1, sd = v2, lower.tail = FALSE)
}

#' @export
scale_type.exceed <- function(x) "exceed"


scale_colour_exceed <- function(...) {

}
