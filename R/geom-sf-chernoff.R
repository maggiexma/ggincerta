geom_sf_chernoff <- function(mapping = aes(),
                             data = NULL,
                             stat = "sf_coordinates",
                             position = "identity",
                             ...,
                             fun.geometry = sf::st_point_on_surface,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  list(
    geom_sf(
      data = data,
      inherit.aes = FALSE,
      fill = NA,
      colour = NA,
      show.legend = FALSE
    ),

    layer_sf(
      geom = get("GeomChernoff", envir = getNamespace("ggChernoff")),
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(fun.geometry = fun.geometry, na.rm = na.rm, ...)
    )
  )
}
