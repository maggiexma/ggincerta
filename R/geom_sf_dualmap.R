geom_sf_dualmap <- function(mapping = NULL,
                            data = NULL,
                            position = "identity",
                            ...,
                            polygon_colour = "white",
                            polygon_linewidth = 0.2,
                            centre_colour = NA,
                            centre_shape = "circle",
                            angle = 0,
                            centre_size = 1,
                            point_fun = sf::st_centroid,
                            uncertainty_scale = scale_fill_gradient(low = "grey95",
                                                                    high = "grey40",
                                                                    name = NULL),
                            value_scale = scale_fill_viridis_c(name = NULL),
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  mapping <- mapping %||% aes()

  fill_expr <- rlang::get_expr(mapping$fill)

  fill_args <- rlang::call_args(fill_expr)
  value_expr <- fill_args[[1]]
  uncert_expr <- fill_args[[2]]

  base_mapping <- mapping
  base_mapping$fill <- NULL

  uncert_mapping <- utils::modifyList(base_mapping, aes(fill = !!uncert_expr))

  value_mapping <- utils::modifyList(base_mapping, aes(fill = !!value_expr))

  uncertainty_scale$name <- rlang::as_label(uncert_expr)
  value_scale$name <- rlang::as_label(value_expr)

  list(
    geom_sf(
      mapping = uncert_mapping,
      data = data,
      position = position,
      ...,
      colour = polygon_colour,
      linewidth = polygon_linewidth,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes
    ),
    uncertainty_scale,
    ggnewscale::new_scale_fill(),
    geom_sf_centre(
      mapping = value_mapping,
      data = data,
      position = position,
      ...,
      colour = centre_colour,
      centre_shape = centre_shape,
      angle = angle,
      size = centre_size,
      point_fun = point_fun,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes
    ),
    value_scale
  )
}
