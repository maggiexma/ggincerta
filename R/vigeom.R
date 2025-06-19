GeomSimplePoint <- ggproto("GeomSimplePoint", Geom,
                           required_aes = c("x", "y"),
                           default_aes = aes(shape = 19, colour = "black"),
                           draw_key = draw_key_point,
                           draw_panel = function(data, panel_params, coord) {
                             
                             coords <- coord$transform(data, panel_params)
                             grid::pointsGrob(
                               coords$x, coords$y,
                               pch = coords$shape,
                               gp = grid::gpar(col = coords$colour)
                             )
                           }
)

geom_simple_point <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSimplePoint, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + 
  geom_simple_point()

GeomSimplePolygon <- ggproto("GeomPolygon", Geom,
                             required_aes = c("x", "y"),
                             
                             default_aes = aes(
                               colour = NA, fill = "grey20", linewidth = 0.5,
                               linetype = 1, alpha = 1
                             ),
                             
                             draw_key = draw_key_polygon,
                             
                             draw_group = function(data, panel_params, coord) {
                               browser()
                               n <- nrow(data)
                               if (n <= 2) return(grid::nullGrob())
                               
                               coords <- coord$transform(data, panel_params)
                               # A polygon can only have a single colour, fill, etc, so take from first row
                               first_row <- coords[1, , drop = FALSE]
                               
                               grid::polygonGrob(
                                 coords$x, coords$y, 
                                 default.units = "native",
                                 gp = grid::gpar(
                                   col = first_row$colour,
                                   fill = scales::alpha(first_row$fill, first_row$alpha),
                                   lwd = first_row$linewidth * .pt,
                                   lty = first_row$linetype
                                 )
                               )
                             }
)
geom_simple_polygon <- function(mapping = NULL, data = NULL, stat = "chull",
                                position = "identity", na.rm = FALSE, show.legend = NA, 
                                inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSimplePolygon, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_simple_polygon(aes(colour = class), fill = NA)

GeomPolygonHollow <- ggproto("GeomPolygonHollow", GeomPolygon,
                             default_aes = aes(colour = "black", fill = NA, linewidth = 0.5, linetype = 1,
                                               alpha = NA)
)
geom_chull <- function(mapping = NULL, data = NULL, 
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, geom = GeomPolygonHollow, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_chull()
