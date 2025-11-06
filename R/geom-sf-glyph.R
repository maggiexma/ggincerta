#' @rdname ggsfgl
#' @export
StatGlyph <- ggproto(
  "StatGlyph",
  StatSf,
  required_aes = c("v1", "v2", "geometry"),
  compute_panel = function(data, scales, coord, size, style, max_v2) {
    data <- StatSf$compute_panel(data, scales, coord)
    data <- sf::st_as_sf(data)
    centroids <- sf::st_centroid(data$geometry)
    coords <- sf::st_coordinates(centroids)
    data$long <- coords[, 1]
    data$lat <- coords[, 2]

    errs <- data$v2
    max_err <- if (is.null(max_v2))
      max(errs, na.rm = TRUE)
    else
      max_v2
    data$theta <- -(errs / max_err) * pi
    data$id <- seq_len(nrow(data))
    data$size <- size
    data$style <- style

    polys <- lapply(seq_len(nrow(data)), function(i) {
      shape_i <- data$style[i]
      if (!shape_i %in% c("icone", "semi"))
        stop("Glyph name not recognised. Must be one of icone or semi.")
      if (shape_i == "icone") {
        x1 <- seq(-3, 3, .05)
        y1 <- sqrt(9 - x1^2)
        cir <- data.frame(x = x1, y = y1)
        x2 <- c(-3, 0, 3)
        y2 <- c(0, -5, 0)
        tri <- data.frame(x = x2, y = y2)
        glyphDat <- rbind(cir, tri)
      } else {
        x1 <- seq(-3, 3, .05)
        y1 <- sqrt(9 - x1^2)
        glyphDat <- data.frame(x = x1, y = y1)
      }

      theta <- data$theta[i]
      R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2)
      Nmat <- R %*% t(glyphDat / data$size[i])
      N <- as.data.frame(t(Nmat))

      data.frame(
        fill = rep(data$v1[i], nrow(glyphDat)),
        glyph = rep(data$v2[i], nrow(glyphDat)),
        group = rep(data$id[i], nrow(glyphDat)),
        x = N$V1 + data$long[i],
        y = N$V2 + data$lat[i],
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, polys)
  }
)

GeomPolygonGlyph <- ggproto(
  "GeomPolygonGlyph",
  GeomPolygon,
  default_aes = c(GeomPolygon$default_aes, aes(glyph = NA))
)

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
#' @inheritParams ggplot2::geom_sf
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
geom_sf_glyph <- function(mapping = NULL,
                          data = NULL,
                          size = 70,
                          style = "icone",
                          max_v2 = NULL,
                          position = "identity",
                          show.legend = TRUE,
                          inherit.aes = TRUE,
                          ...) {


  needs_geometry <- is.null(rlang::get_expr(mapping$geometry))
  if (needs_geometry) {
    geom_col <- NULL
    if (!is.null(data) && inherits(data, "sf")) {
      geom_col <- attr(data, "sf_column")
    } else {
      geom_col <- "geometry"
    }
    mapping$geometry <- rlang::sym(geom_col)
  }

  v1_title <- rlang::as_label(mapping$v1)
  v2_title <- rlang::as_label(mapping$v2)

  c(
    layer(
      stat = StatGlyph,
      data = data,
      mapping = mapping,
      geom = GeomPolygonGlyph,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        size = size,
        style = style,
        max_v2 = max_v2,
        ...
      )
    ),
    coord_sf(),
    scale_fill_distiller(
      name = v1_title,
      palette = "Oranges",
      direction = 1,
      guide = guide_colorbar(order = 1)
    ),
    scale_glyph_continuous(
      name = v2_title,
      order = 2,
      style = style
    )
  )
}
