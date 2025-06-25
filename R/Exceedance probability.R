library(ggplot2)

nc$sd <- abs(rnorm(nrow(nc), mean = 1, sd = 0.3))

StatExceed <- ggproto("StatExceed", Stat,
                      required_aes = c("geometry","estimate","error"),
                      compute_panel = function(data, scales,
                                               dist_fun = stats::pnorm,
                                               threshold, coord) {

                        data <- StatSf$compute_panel(data, scales, coord)

                        if (!"pr_exc" %in% names(data)) {

                          data$pr_exc <- dist_fun(q = threshold,
                                                  mean = data$estimate,
                                                  sd = data$error,
                                                  lower.tail  = FALSE)
                        }
                        data
                      }
)

stat_sf_exceed <- function(mapping = NULL,
                           data = NULL,
                           geom = "sf",
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           dist_fun = stats::pnorm,
                           threshold,
                           palette = "YlOrRd",
                           direction = 1,
                           ...) {
  layer <- layer_sf(
    stat = StatExceed,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      dist_fun = dist_fun,
      threshold = threshold,
      coord = coord_sf(),
      ...
    )
  )

  legend_title <- paste0("Pr[X > ", threshold, "]")
  scale <- scale_fill_distiller(
    palette = palette,
    direction = direction,
    name = legend_title
  )
  list(layer, scale, coord_sf())
}

ggplot(nc) +
  stat_sf_exceed(mapping = aes(geometry = geometry, estimate = value, error = sd,
                               fill = after_stat(pr_exc)), threshold = 0.5, dist_fun = stats::pnorm) +
  coord_sf()
