StatGlyph <- ggproto(
  "StatGlyph",
  Stat,
  required_aes = c("geometry", "estimate", "error"),
  default_aes  = aes(glyph = NA_real_),
  compute_panel = function(data,
                           scales,
                           size = 70,
                           glyph = "icone",
                           max_error = NULL) {
    centroids <- sf::st_centroid(data$geometry)
    coords <- sf::st_coordinates(centroids)
    data$long <- coords[, 1]
    data$lat <- coords[, 2]

    errs <- data$error
    max_err <- if (is.null(max_error))
      max(errs, na.rm = TRUE)
    else
      max_error
    data$theta <- -(errs / max_err) * pi

    data$id <- seq_len(nrow(data))
    data$size <- size
    data$glyph <- glyph

    polys <- lapply(seq_len(nrow(data)), function(i) {
      shape_i <- data$glyph[i]
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
        val = rep(data$estimate[i], nrow(glyphDat)),
        err = rep(data$error[i], nrow(glyphDat)),
        id = rep(data$id[i], nrow(glyphDat)),
        long = N$V1 + data$long[i],
        lat = N$V2 + data$lat[i],
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, polys)
  }
)

GeomPolygonGlyph <- ggproto(
  "GeomPolygonGlyph",
  GeomPolygon,
  default_aes = utils::modifyList(GeomPolygon$default_aes, aes(glyph = NA_real_))
)

geom_sf_glyph <- function(mapping = NULL,
                          data = NULL,
                          size = 70,
                          glyph = "icone",
                          max_error = NULL,
                          position = "identity",
                          show.legend = TRUE,
                          inherit.aes = TRUE,
                          ...) {
  extra <- aes(
    x = after_stat(long),
    y = after_stat(lat),
    group = after_stat(id),
    fill = after_stat(val),
    glyph = after_stat(err)
  )
  mapping <- utils::modifyList(mapping %||% aes(), extra)

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
        glyph = glyph,
        max_error = max_error,
        ...
      )
    ),
    coord_sf()
  )
}

ggplot(nc) +
  geom_sf_glyph(aes(geometry = geometry, estimate = value, error = sd), size = 50, glyph = "icone") +
  scale_fill_viridis_c(name = "value", guide = guide_colorbar(order = 1)) +
  scale_glyph_continuous(name = "sd", order = 2) +
  theme(legend.position = "right", legend.box = "horizontal")
