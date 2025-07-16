geom_point_bivariate <- function(mapping = NULL,
                                 data = NULL,
                                 position = "identity",
                                 na.rm = FALSE,
                                 show.legend = TRUE,
                                 inherit.aes = TRUE,
                                 terciles = TRUE,
                                 flipAxis = FALSE,
                                 ...) {
  layer(
    stat = StatBivariate,
    data = data,
    mapping = mapping,
    geom = 'point',
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      terciles = terciles,
      flipAxis = flipAxis,
      ...
    )
  )
}
