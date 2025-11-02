#' @rdname ggsfpi
#' @export
StatPixel <- ggproto("StatPixel",
                     StatSf,
                     required_aes = c("v1", "v2"),
                     compute_panel = function(data,
                                              scales,
                                              coord,
                                              n,
                                              distribution
                                              ){

                       data <- StatSf$compute_panel(data, scales, coord)
                       sf_data <- sf::st_as_sf(data)

                       full_grid <- sf::st_make_grid(sf_data, n = n)

                       pixel_fun <- function(i) {
                         grid <- sf::st_intersection(full_grid, sf_data[i, ])
                         grid <- suppressWarnings(sf::st_cast(grid, "POLYGON"))
                         grid <- sf::as_Spatial(grid)
                         grid$ID <- rep(i, length(grid))
                         return(grid)
                       }

                       list_grids <- lapply(1:nrow(sf_data), pixel_fun)
                       all_grids <- do.call(rbind, list_grids)
                       pixel_df <- SpatialPolygonsDataFrame_to_df(all_grids)
                       pixel_df$`id.1` <- NULL
                       colnames(pixel_df) <- c("x", "y", "group", "ID")
                       sf_data$ID <- 1:nrow(sf_data)
                       pixel_df$v1 <- sf_data$v1[match(pixel_df$ID, sf_data$ID)]
                       pixel_df$v2 <- sf_data$v2[match(pixel_df$ID, sf_data$ID)]

                       createU <- function(x, v1, v2) {
                         up <- unique(v1[x]) + unique(v2[x])
                         lo <- unique(v1[x]) - unique(v2[x])

                         vec <- seq(lo, up, length.out = 5)
                         values <- sample(vec, length(x), replace = TRUE)
                         values
                       }

                       createN <- function(x, v1, v2){
                         mean <- unique(v1[x])
                         sd <- unique(v2[x])
                         values <- rnorm(length(x), mean = mean, sd = sd)
                         values
                       }

                       createPixrv <- function(pixelGeo, distribution, q) {


                         pixel_distinct <- pixelGeo[!duplicated(pixelGeo$group), ]

                         if (distribution == "uniform") {
                           rvarray <- tapply(
                             1:nrow(pixel_distinct),
                             pixel_distinct$ID,
                             createU,
                             v1 = pixel_distinct$v1,
                             v2 = pixel_distinct$v2
                           )

                         } else if (distribution == "normal") {
                           rvarray <- tapply(
                             1:nrow(pixel_distinct),
                             pixel_distinct$ID,
                             createN,
                             v1 = pixel_distinct$v1,
                             v2 = pixel_distinct$v2
                           )

                         } else {
                           stop("Unknown distribution type.")
                         }

                         output_data <- data.frame(ID = rep(names(rvarray), lengths(rvarray)),
                                                   group = unname(unlist(tapply(pixel_distinct$group,
                                                                                pixel_distinct$ID,
                                                                                list))),
                                                   fill = unname(unlist(rvarray)),
                                                   stringsAsFactors = FALSE)
                         geo <- tapply(1:nrow(pixelGeo),
                                       pixelGeo$group,
                                       function(i) list(as.matrix(cbind(pixelGeo$x[i], pixelGeo$y[i]))))
                         geo_data <- list(group = names(geo),
                                          geometry = lapply(geo, function(x) sf::st_polygon(list(unname(x)))))
                         class(geo_data) <- "data.frame"
                         attr(geo_data, "row.names") <- seq_along(geo)

                         merge(output_data, geo_data, by = c("group"))
                       }
                       out <- createPixrv(pixel_df, distribution, q)
                       sf::st_as_sf(out, sf_column_name = "geometry")
                     }
)

#' Generate pixel maps on sf objects
#'
#' `geom_sf_pixel()` adds a pixel map layer based on simple feature (sf) objects
#' to a ggplot. In a pixel map, each region is divided into small pixels, with
#' colours mapped from values sampled from distribution specified.
#'
#' @section Pixel map layer contents:
#' The layer returned by `geom_sf_pixel()` internally includes a scale object
#' created by `scale_fill_distiller()`.
#' Therefore, modifying the scale will trigger a warning indicating that
#' the scale for `fill` is being replaced.
#'
#' @inheritParams ggplot2::geom_sf
#' @inheritParams sf::st_make_grid
#' @param mapping Set of aesthetic mappings created by [aes()].
#'   `v1` and `v2` are required, which are the variables used as the parameters
#'   in the sampling distribution.
#' @param distribution Sampling distribution: `"uniform"`(the default) or, `"normal"`.
#'   * `"uniform"` treats `v1` as the centre and uniformly samples within the range
#'     `v1 ± v2`.
#'   * `"normal"` treats `v1` as the mean and `v2` as the standard deviation.
#'
#' @examples
#' data(nc)
#' ggplot(nc) + geom_sf_pixel(mapping = aes(v1 = value, v2 = sd))
#'
#' @rdname ggsfpi
#' @export
geom_sf_pixel <- function(mapping = NULL,
                          data = NULL,
                          n = 40,
                          distribution = "uniform",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {


  mapping[["fill"]] <- NA

  list(layer_sf(
    data = data,
    mapping = mapping,
    stat = StatPixel,
    geom = 'sf',
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  n = n,
                  distribution = distribution,
                  ...)
    ),
    geom_sf(fill = NA, color = 'black'),
    scale_fill_distiller(palette = "Oranges", direction = 1),
    coord_sf())
}


# from FRK
SpatialPolygonsDataFrame_to_df <- function (sp_polys, vars = names(sp_polys))
{
  if (!(is(sp_polys, "SpatialPolygonsDataFrame") | is(sp_polys,
                                                      "SpatialPixelsDataFrame")))
    stop("sp_polys needs to be of class SpatialPolygonsDataFrame\n                 or SpatialPixelsDataFrame")
  if (is(sp_polys, "SpatialPixelsDataFrame"))
    sp_polys <- as(sp_polys, "SpatialPolygonsDataFrame")
  polynames <- as.character(row.names(sp_polys))
  list_polys <- lapply(1:length(sp_polys), function(i) {
    coords <- sp_polys@polygons[[i]]@Polygons[[1]]@coords
    row.names(coords) <- NULL
    coords <- data.frame(coords)
    poldf <- cbind(coords, id = polynames[i], stringsAsFactors = FALSE)
    rownames(poldf) <- NULL
    poldf
  })
  df_polys <- dplyr::bind_rows(list_polys)
  df_polys$id <- as.character(df_polys$id)
  sp_polys$id <- row.names(sp_polys)
  cnames <- sp::coordnames(sp_polys)
  vars_no_coords <- vars[which(!vars %in% cnames)]
  if (length(vars_no_coords) > 0)
    df_polys <- dplyr::left_join(df_polys, sp_polys@data[c("id",
                                                    vars_no_coords)], by = "id")
  df_polys
}
