library(ggplot2)

StatPointExceed <- ggproto("StatPointExceed", Stat,
                           required_aes = c("x", "y", "estimate", "error"),

                           compute_panel = function(data, scales, dist_fun, threshold, na.rm = FALSE) {
                             data$pr_exc <- dist_fun(
                               q          = threshold,
                               mean       = data$estimate,
                               sd         = data$error,
                               lower.tail = FALSE
                             )
                             data
                           }
)

stat_point_exceed <- function(mapping     = NULL,
                              data        = NULL,
                              geom        = "point",
                              position    = "identity",
                              show.legend = NA,
                              inherit.aes = TRUE,
                              dist_fun    = stats::pnorm,
                              threshold,
                              palette     = "YlOrRd",
                              direction   = 1,
                              na.rm       = FALSE,
                              ...) {

  layer_pt <- layer(
    stat        = StatPointExceed,
    data        = data,
    mapping     = mapping,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = c(
      list(
        dist_fun  = dist_fun,
        threshold = threshold,
        na.rm     = na.rm
      ),
      list(...)
    )
  )

  legend_title <- paste0("Pr[X > ", threshold, "]")
  scale_pt <- scale_fill_distiller(
    palette   = palette,
    direction = direction,
    name = legend_title
  )

  list(layer_pt, scale_pt)
}

ggplot(nc_centroids,
       aes(x = x, y = y, estimate = estimate, error = error)) +
  stat_point_exceed(
    mapping   = aes(fill = after_stat(pr_exc)),
    threshold = 0.5,
    palette = "YlOrRd",
    size = 4,
    shape = 21
  ) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1
  ) +
  coord_equal() +
  theme_minimal()
