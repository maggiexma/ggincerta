geom_point_exceed <- function(mapping = NULL,
                              data = NULL,
                              dist_fun = stats::pnorm,
                              threshold,
                              palette = "YlOrRd",
                              direction = 1,
                              geom = "point",
                              position = "identity",
                              show.legend = NA,
                              inherit.aes = TRUE,
                              na.rm = FALSE,
                              ...) {
  full_map <- modifyList(mapping, aes(fill = after_stat(pr_exc)))

  layer_pt <- layer(
    stat = StatExceed,
    data = data,
    mapping = full_map,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      dist_fun = dist_fun,
      threshold = threshold,
      na.rm = na.rm,
      ...
    )
  )

  legend_title <- sprintf("Pr[X > %s]", threshold)
  scale_pt <- scale_fill_distiller(name = legend_title,
                                   palette = palette,
                                   direction = direction)

  list(layer_pt, scale_pt)
}
