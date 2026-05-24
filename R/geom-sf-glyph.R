parse_glyph_mapping <- function(mapping) {
  mapping <- mapping %||% aes()

  list(
    mapping = mapping,
    has_angle = !is.null(mapping$angle),
    has_smile = !is.null(mapping$smile)
  )
}

#' Glyph map
#'
#' `geom_sf_glyph()` generates a glyph map sf layer. A glyph map is a
#' centroid-based map, where each region is represented by a chosen glyph.
#'
#' Regular shape glyphs can be treated as ordinary point-like symbols and used
#' together with `bivariate_scale()`. Drop-shaped glyphs use rotation angle to
#' represent uncertainty. Chernoff glyphs are adapted from the `ggChernoff`
#' package and work with the `smile` aesthetic.
#'
#' @inheritParams ggplot2::geom_sf
#' @param shape Glyph shape. One of `"circle"` (the default), `"square"`,
#'   `"triangle"`, `"hex"`, `"drop"`, or `"chernoff"`.
#' @param max_angle Maximum value of the `angle` aesthetic used for rescaling
#'   glyph rotation.
#' @param size A positive numeric scaling factor controlling glyph size.
#' @param point_fun Function used to calculate the representative point for each
#'   region. The default is usually [sf::st_point_on_surface()].
#' @param border_colour Colour used for glyph borders.
#' @param angle_guide Logical indicating whether to display a guide for the
#'   `angle` aesthetic.
#' @param angle_name Title used for the angle guide.
#' @param angle_order Order of the angle guide relative to other guides.
#' @returns A list of ggplot2 layer objects.
#' @examples
#' # Regular glyph map
#' ggplot(nc) +
#'   geom_sf_glyph(aes(colour = value), shape = "hex")
#'
#' # Rotated drop glyph map
#' ggplot(nc) +
#'   geom_sf_glyph(
#'     aes(colour = value, angle = sd),
#'     shape = "drop"
#'   )
#'
#' # Chernoff face glyph map
#' if (requireNamespace("ggChernoff", quietly = TRUE)) {
#'   ggplot(nc) +
#'     geom_sf_glyph(
#'       aes(colour = value, smile = sd),
#'       shape = "chernoff"
#'     )
#' }
#' @export
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
                          angle_order = 99) {
  parsed <- parse_glyph_mapping(mapping)
  mapping <- parsed$mapping

  if (parsed$has_angle && shape != "drop") {
    cli::cli_warn("{.aes angle} is only used when {.code shape = 'drop'}.")
    mapping$angle <- NULL
  }

  if (parsed$has_smile && shape != "chernoff") {
    cli::cli_warn("{.aes smile} is only used when {.code shape = 'chernoff'}.")
    mapping$smile <- NULL
  }

  if (shape == "chernoff") {
    return(list(
      geom_sf_chernoff(
        mapping = mapping,
        data = data,
        ...,
        fun.geometry = point_fun,
        na.rm = na.rm,
        show.legend = show.legend,
        inherit.aes = inherit.aes
      ),
      scale_smile_continuous()
    ))
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

  if (!parsed$has_angle ||
      shape != "drop" || !isTRUE(angle_guide)) {
    return(layer)
  }

  angle_label <- if (is_waiver(angle_name)) {
    rlang::as_label(mapping$angle)
  } else {
    angle_name
  }

  list(layer,
       scale_angle_continuous(name = angle_label, order = angle_order))
}
