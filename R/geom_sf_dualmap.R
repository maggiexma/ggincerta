parse_dualmap_mapping <- function(mapping) {
  mapping <- mapping %||% aes()

  outer_mapping <- mapping
  inner_mapping <- mapping

  outer_mapping$colour <- NULL
  outer_mapping$angle <- NULL
  outer_mapping$smile <- NULL

  inner_mapping$fill <- NULL

  list(outer = outer_mapping, inner = inner_mapping)
}

geom_sf_dualmap <- function(mapping = NULL,
                            data = NULL,
                            ...,
                            shape = "circle",
                            angle = 0,
                            max_angle = NULL,
                            size = 1,
                            point_fun = sf::st_point_on_surface,
                            border_colour = NA,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            angle_guide = TRUE,
                            angle_name = waiver(),
                            angle_order = 2,
                            fill_scale = NULL) {
  parsed <- parse_dualmap_mapping(mapping)

  layers <- list(
    geom_sf(
      mapping = parsed$outer,
      data = data,
      ...,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes
    ),

    geom_sf_glyph(
      mapping = parsed$inner,
      data = data,
      ...,
      shape = shape,
      angle = angle,
      max_angle = max_angle,
      size = size,
      point_fun = point_fun,
      border_colour = border_colour,
      na.rm = na.rm,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      angle_guide = angle_guide,
      angle_name = angle_name,
      angle_order = angle_order
    )
  )

  if (is.null(fill_scale) &&
      !is.null(parsed$outer$fill)) {
    fill_expr <- rlang::get_expr(parsed$outer$fill)

    is_duo <- rlang::is_call(fill_expr, "duo")

    if (!is_duo) {
      layers <- c(layers, list(scale_fill_gradient(
        low = "grey95", high = "grey40"
      )))
    }
  }

  layers
}
