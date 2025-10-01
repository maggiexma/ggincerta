StatExceed <- ggplot2::ggproto(
  "StatExceed",
  ggplot2::StatSf,
  required_aes = c("estimate", "error"),
  compute_panel = function(self,
                           data,
                           scales,
                           coord,
                           dist_fun = stats::pnorm,
                           threshold) {
    ok <- is.finite(data$error) & data$error > 0 &
      is.finite(data$estimate) & is.finite(threshold)

    data$pr_exc <- NA_real_
    if (any(ok)) {
      data$pr_exc[ok] <- dist_fun(
        q = threshold,
        mean = data$estimate[ok],
        sd   = data$error[ok],
        lower.tail = FALSE
      )
    }

    ggplot2::ggproto_parent(ggplot2::StatSf, self)$compute_panel(data, scales, coord)
  }
)

geom_sf_exceed <- function(mapping = NULL,
                           data = NULL,
                           dist_fun = stats::pnorm,
                           threshold,
                           palette = "YlOrRd",
                           direction = 1,
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           na.rm = FALSE,
                           ...) {
  base_map <- ggplot2::aes(geometry = geometry, fill = after_stat(pr_exc))
  if (is.null(mapping))
    mapping <- ggplot2::aes()
  mapping <- utils::modifyList(base_map, mapping)

  layer_sf <- ggplot2::layer(
    stat = StatExceed,
    data = data,
    mapping = mapping,
    geom = "sf",
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
  scale_fill <- ggplot2::scale_fill_distiller(name = legend_title,
                                              palette = palette,
                                              direction = direction)

  list(layer_sf, scale_fill)
}
