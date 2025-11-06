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
