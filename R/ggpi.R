library(sf)
library(reshape2)
library(FRK)
library(ggplot2)

names(nc)[ names(nc) == "NAME" ] <- "id"
names(nc)[ names(nc) == "value" ] <- "estimate"
names(nc)[ names(nc) == "sd" ] <- "error"

StatPixelatedPolygon <- ggproto(
  "StatPixelatedPolygon", Stat,
  required_aes = c("estimate","error","geometry","id"),

  compute_panel = function(data, scales, coord,
                           id_col = "id",
                           distribution = "uniform",
                           pixelSize = 40) {

    sf_data <- sf::st_as_sf(data, sf_column_name = "geometry")

    full_grid <- st_make_grid(sf_data, n = pixelSize)

    pixel_fun <- function(x) {
      grid <- st_intersection(full_grid, sf_data[x, ])
      grid <- suppressWarnings(st_cast(grid, "POLYGON"))
      grid <- as_Spatial(grid)
      grid$ID <- rep(sf_data[x, id_col][[1]], length(grid))
      return(grid)
    }

    list_grids <- lapply(1:nrow(sf_data), pixel_fun)
    all_grids <- do.call(rbind, list_grids)
    pixel_df <- SpatialPolygonsDataFrame_to_df(all_grids)
    pixel_df$`id.1` <- NULL
    colnames(pixel_df) <- c("long", "lat", "group", id_col)
    pixel_df$ID <- cumsum(!duplicated(pixel_df[[id_col]]))
    print(class(pixel_df))

    pixel_df[[id_col]] <- as.character(pixel_df[[id_col]])
    sf_data[[id_col]] <- as.character(sf_data[[id_col]])
    pixel_df$estimate <- sf_data$estimate[match(pixel_df[[id_col]], sf_data[[id_col]])]
    pixel_df$error <- sf_data$error[match(pixel_df[[id_col]], sf_data[[id_col]])]

    createU <- function(x, est, error){

      up <- unique(est[x]) + unique(error[x])
      lo <- unique(est[x]) - unique(error[x])

      vec <- seq(lo, up, length.out=5)
      values <- sample(vec, length(x), replace=TRUE)
      values

    }

    createPixrv <- function(pixelGeo, distribution){

      if(distribution == "uniform")

        rvarray <- tapply(1:nrow(pixelGeo), pixelGeo$ID, function(x, est, error) createU(x, est, error),
                          est = pixelGeo$estimate, error = pixelGeo$error)
      else
        if(distribution == "normal")
          rvarray <- tapply(1:nrow(pixelGeo), pixelGeo$ID, function(x, est, error) createN(x, est, error),
                            est = pixelGeo$estimate, error = pixelGeo$error)
        else
          if(distribution == "discrete")
            rvarray <- tapply(1:nrow(pixelGeo), pixelGeo$ID, function(x, q) createD(x, q), q = pixelGeo$q)

          rvmelt <- melt(rvarray)
          values <- unlist(rvmelt$value)
          output_data <- pixelGeo
          output_data$values <- values
          output_data
    }


    output_data <- createPixrv(pixel_df, distribution)
    output_data
  }
)

geom_pixelmap <- function(mapping = aes(geometry),
                          data = NULL,
                          pixelSize = 40,
                          distribution = "uniform",
                          id_col = "id",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {

  df_in <- as.data.frame(data)[ , c(id_col, "estimate", "error", as_label(mapping$geometry))]
  names(df_in)[4] <- "geometry"
  out_df <- StatPixelatedPolygon$compute_panel(
    data = df_in,
    scales = list(),
    coord = coord_sf(),
    id_col = id_col,
    distribution = distribution,
    pixelSize = pixelSize
  )

  layer(
    data = out_df,
    mapping = aes(x = long, y = lat, group = group, fill = values),
    stat = "identity",
    geom = "polygon",
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot() +
  geom_pixelmap(
    data = nc,
    mapping = aes(geometry = geometry),
    id_col = "id",
    pixelSize = 40,
    distribution = "uniform"
  ) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_sf(data = nc, fill = NA, color = 'black', size = 0.2)
