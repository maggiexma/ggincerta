StatExceed <- ggproto("StatExceed",
                      Stat,
                      required_aes = c("estimate", "error"),
                      compute_panel = function(data,
                                               scales,
                                               dist_fun = stats::pnorm,
                                               threshold,
                                               coord) {
                        if ("geometry" %in% names(data)) {
                          data <- StatSf$compute_panel(data, scales, coord)
                        }

                        data$pr_exc <- dist_fun(q = threshold,
                                                mean = data$estimate,
                                                sd = data$error,
                                                lower.tail = FALSE)
                        data
                      }

)

#' @title Geom layer function for exceedance probability map on sf objects
#' @description
#' geom_sf_exceed applies StatExceed to sf data and returns a map that visualises
#' the probability of exceeding some nominated threshold of concern.
#'
#' @param mapping Aesthetic mapping that must include \code{geometry}, \code{estimate}, and \code{error}.
#' @param data An sf object.
#' @param geom Geom type to use.
#' @param position Position adjustment.
#' @param show.legend Logical; whether to display a legend.
#' @param inherit.aes Logical; whether to inherit global aesthetics.
#' @param dist_fun Function to compute tail probabilities.
#' @param threshold Numeric threshold for exceedance probability.
#' @param palette Palette name for \code{scale_fill_distiller}.
#' @param direction Direction of the palette (1 or -1).
#' @param ... Additional parameters passed to \code{layer_sf()}.
#'
#' @examples
#' set.seed(10086)
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
#'   dplyr::mutate(value = rnorm(n()), sd = rnorm(n()))
#' nc$sd <- abs(rnorm(nrow(nc), mean = 1, sd = 0.3))
#' ggplot(nc) +
#'   geom_sf_exceed(
#'     mapping = aes(
#'       geometry = geometry,
#'       estimate = value,
#'       error    = sd,
#'       fill     = after_stat(pr_exc)
#'     ),
#'     threshold = 0.5,
#'     dist_fun  = stats::pnorm
#'   ) +
#'   coord_sf()
#'
#' @return A list containing a ggproto object drawing sf geometries colored by
#' exceedance probability and a ggplot2 scale object for the fill aesthetic.
#'
#' @import ggplot2
#' @export
geom_sf_exceed <- function(mapping = NULL,
                           data = NULL,
                           geom = "sf",
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           dist_fun = stats::pnorm,
                           threshold,
                           palette = "YlOrRd",
                           direction = 1,
                           ...) {
  layer <- layer_sf(
    stat = StatExceed,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      dist_fun = dist_fun,
      threshold = threshold,
      coord = coord_sf(),
      ...
    )
  )

  legend_title <- paste0("Pr[X > ", threshold, "]")
  scale <- scale_fill_distiller(palette = palette,
                                direction = direction,
                                name = legend_title)
  list(layer, scale, coord_sf())
}
