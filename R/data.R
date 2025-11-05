#' North Carolina SIDS data
#'
#' @description
#' The datasets `nc` and `nc_centroids` are derived from the North Carolina
#' shapefile (`nc.shp`) included in the \pkg{sf} package. Two random variables,
#' `value` and `sd`, have been added for demonstration purposes, and the
#' geometries in `nc_centroids` have been converted to centroids.
#'
#' Further details about the original data can be found in the
#' [spdep package vignette](https://r-spatial.github.io/spdep/articles/sids.html).
#'
#' @format A `sf` object.
#'
#' @examples
#' head(nc)
#'
#' plot(sf::st_geometry(nc_centroids))
"nc"

#' @rdname nc
"nc_centroids"
