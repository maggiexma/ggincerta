parse_glyph_mapping <- function(mapping) {
  mapping <- mapping %||% aes()

  list(
    mapping = mapping,
    has_angle = !is.null(rlang::get_expr(mapping$angle)),
    has_smile = !is.null(rlang::get_expr(mapping$smile))
  )
}

#' Generate glyph maps on sf objects
#'
#' `geom_sf_glyph()` adds a glyph map layer based on simple feature (sf) objects
#' to a ggplot. A glyph map is essentially a centroid-based map, where each
#' region is represented by a rotated glyph, and the rotation angle indicates
#' the value of `v2` specified in the mapping.
#'
#' @section Glyph map layer contents:
#' The layer returned by `geom_sf_glyph()` actually contains two scales,
#' corresponding to the two variables specified in the mapping.
#' Therefore, modifying the scale for `v1` will trigger a warning
#' indicating that the scale for `fill` is being replaced.
#'
#' @inheritParams geom_sf
#' @param mapping Set of aesthetic mappings created by [aes()].
#'   `v1` and `v2` are required, which are the variables used for glyph fill
#'   and rotation, respectively.
#' @param size A positive numeric scaling factor controlling glyph size.
#'   Larger values produce smaller glyphs.
#' @param style Either `"icone"` or `"semi"`. Controls the glyph shape.
#' @param max_v2 Numeric value setting the upper limit for `v2`.
#'
#' @returns A list of ggplot2 layer objects.
#'
#' @examples
#' # Basic glyph map
#' p <- ggplot(nc) + geom_sf_glyph(mapping = aes(v1 = value, v2 = sd))
#' p1 <- ggplot(nc) + geom_sf_glyph(mapping = aes(v1 = value, v2 = sd), style = "semi")
#'
#' # Customize labels and theme
#' p + labs(title = "glyph map on nc") + theme(legend.position = "left", legend.box = "horizontal")
#'
#' # Replacing the internal fill scale triggers a message
#' # ("Scale for fill is already present. Adding another scale for fill...")
#' p + scale_fill_distiller(palette = "Blues")
#'
#' @rdname ggsfgl
#' @export
#'
geom_sf_glyph <- function(mapping = NULL,
                          data = NULL,
                          ...,
                          shape = "circle",
                          max_angle = NULL,
                          size = 1,
                          point_fun = sf::st_point_on_surface,
                          border_colour = NA,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          angle_guide = TRUE,
                          angle_name = waiver(),
                          angle_order = 2) {
  parsed <- parse_glyph_mapping(mapping)
  mapping <- parsed$mapping

  if (shape == "chernoff") {
    return(
      geom_sf_chernoff(
        mapping = mapping,
        data = data,
        ...,
        fun.geometry = point_fun,
        na.rm = na.rm,
        show.legend = show.legend,
        inherit.aes = inherit.aes
      )
    )
  }

  if (parsed$has_smile) {
    cli::cli_warn(
      "{.aes smile} is only used when {.code shape = 'chernoff'}."
    )
  }

  layer <- geom_sf_pin(
    mapping = mapping,
    data = data,
    ...,
    shape = shape,
    max_angle = max_angle,
    size = size,
    point_fun = point_fun,
    border_colour = border_colour,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )

  if (!parsed$has_angle || !isTRUE(angle_guide)) {
    return(layer)
  }

  angle_label <- if (is_waiver(angle_name)) {
    rlang::as_label(mapping$angle)
  } else {
    angle_name
  }

  list(
    layer,
    scale_angle_continuous(
      name = angle_label,
      order = angle_order
    )
  )
}
