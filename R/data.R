#' North Carolina SIDS data
#'
#' @description
#' The dataset `nc` is derived from the North Carolina shapefile (`nc.shp`)
#' included in the \pkg{sf} package. Two random variables, `value` and `sd`,
#' have been added for demonstration purposes.
#'
#' Further details about the original data can be found in the
#' [spdep package vignette](https://r-spatial.github.io/spdep/articles/sids.html).
#'
#' @format A `sf` object.
#'
#' @examples
#' head(nc)
#'
#' plot(sf::st_geometry(nc))
"nc"

#' Simulated North Carolina spatial patterns
#'
#' @description
#' A simulated areal dataset based on the North Carolina county boundaries.
#' It contains three spatial patterns, linear trend, hotspot, and CAR smooth
#' field, each generated at medium and strong signal strengths.
#'
#' @format An `sf` object with 600 rows. It contains the original variables
#' from `nc`, together with:
#' \describe{
#'   \item{pattern}{Spatial pattern type.}
#'   \item{strength}{Signal strength.}
#'   \item{noise_level}{Noise level used in the simulation.}
#'   \item{value_sim}{Simulated value.}
#' }
#'
#' @examples
#' head(nc_sim)
#'
#' plot(sf::st_geometry(nc_sim))
"nc_sim"
