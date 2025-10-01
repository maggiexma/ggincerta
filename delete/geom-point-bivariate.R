StatBivariate <- ggplot2::ggproto(
  "StatBivariate",
  ggplot2::StatSf,
  required_aes = c("estimate", "error"),
  compute_panel = function(self,
                           data,
                           scales,
                           coord,
                           terciles = TRUE,
                           flipAxis = FALSE) {
    x <- data$estimate
    y <- data$error

    qx <- stats::quantile(x, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE)
    qy <- stats::quantile(y, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE)
    if (!terciles || length(unique(qx)) < 4)
      qx <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 4)
    if (!terciles || length(unique(qy)) < 4)
      qy <- seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE), length.out = 4)

    est_bin <- cut(x,
                   breaks = qx,
                   include.lowest = TRUE,
                   labels = FALSE)
    err_bin <- cut(y,
                   breaks = qy,
                   include.lowest = TRUE,
                   labels = FALSE)

    combo <- (est_bin - 1L) * 3L + err_bin
    data$fill <- factor(combo, levels = 1:9)

    attr(data$fill, "bivar_breaks") <- list(
      est_breaks = qx,
      err_breaks = qy,
      n_est = 3L,
      n_err = 3L
    )
    ggplot2::ggproto_parent(ggplot2::StatSf, self)$compute_panel(data, scales, coord)
  }
)

geom_sf_bivariate <- function(mapping = NULL,
                              data = NULL,
                              position = "identity",
                              show.legend = TRUE,
                              inherit.aes = TRUE,
                              ...) {
  base_map <- ggplot2::aes(geometry = geometry, fill = after_stat(fill))
  if (is.null(mapping))
    mapping <- ggplot2::aes()
  mapping <- utils::modifyList(base_map, mapping)

  c(layer(
    stat = StatBivariate,
    data = data,
    mapping = mapping,
    geom = "sf",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  ),
  coord_sf(),
  scale_fill_bivariate())
}

ggplot() +
  geom_sf_bivariate(
    data = nc_centroids,
    aes(estimate = value, error = sd),
    size = 3,
    shape = 21
  ) +
  theme_minimal()
