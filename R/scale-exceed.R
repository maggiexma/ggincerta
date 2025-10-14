ScaleExceed <- ggproto(
  "ScaleExceed",
  ScaleContinuous,

  transform = function(self, x) {
    browser()
    v1 <- vapply(x, function(e) e$v1, numeric(1))
    v2 <- vapply(x, function(e) e$v2, numeric(1))
    p <- self$dist_fun(x = self$threshold, v1 = v1, v2 = v2)
    return(p)
  }
)

scale_fill_exceed <- function(
    name = NULL,
    dist_fun,
    threshold,
    limits = c(0, 1),
    na.value = NA,
    guide = "colourbar",
    ...
) {
  pal <- scales::gradient_n_pal(viridisLite::viridis(256))

  sc <- continuous_scale(
    aesthetics = "fill",
    scale_name = "exceed",
    palette  = pal,
    limits   = limits,
    na.value = na.value,
    guide    = guide,
    super    = ScaleExceed,
    ...
  )
  sc$dist_fun  <- dist_fun
  sc$threshold <- threshold
  sc$name      <- name
  sc
}


#' @export
scale_type.exceed <- function(x) "exceed"


scale_colour_exceed <- function(...) {

}
