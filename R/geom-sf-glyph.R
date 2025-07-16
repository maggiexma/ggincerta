StatGlyph <- ggproto("StatGlyph",
                     Stat,
                     required_aes = c("geometry", "estimate", "error"),
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

#' @title Geom layer function for glyph map on sf objects
#' @description
#' \code{geom_sf_glyph} applies \code{StatGlyph} to sf data and returns a centroid map
#'  with color and rotation based on estimate and error values.
#'
#' @param mapping Aesthetic mapping that must include \code{geometry}, \code{estimate}, and \code{error}.
#' @param data An sf object.
#' @param geom Geom type, default "polygon"
#' @param position Position adjustment.
#' @param show.legend Logical; whether to display a legend.
#' @param inherit.aes Logical; whether to inherit global aesthetics.
#' @param size Glyph scale factor.
#' @param glyph Glyph type: "icone" or "semi".
#' @param max_error Maximum error scale.
#' @param ... Other params passed to layer()
#'
#' @examples
#'set.seed(10086)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
#'   mutate(value = rnorm(n()), sd = rnorm(n()))
#'
#' ggplot(nc, aes(geometry = geometry, estimate = value, error = sd)) +
#'   geom_sf_glyph(size = 50, glyph = "icone") +
#'   coord_sf() + theme_void()
#'
#' @import sf
#' @import ggplot2
#' @export
geom_sf_glyph <- function(mapping = NULL,
                          data = NULL,
                          position = "identity",
                          show.legend = NA,
                          inherit.aes = TRUE,
                          size = 70,
                          glyph = "icone",
                          max_error = NULL,
                          ...) {
  extra_aes <- aes(
    x = after_stat(long),
    y = after_stat(lat),
    group = after_stat(id),
    fill = after_stat(val)
  )

  layer(
    stat = StatGlyph,
    data = data,
    mapping = extra_aes,
    geom = 'polygon',
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size = size,
      glyph = glyph,
      max_error = max_error,
      ...
    )
  )
}

