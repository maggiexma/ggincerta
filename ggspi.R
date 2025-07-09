library(ggplot2)
library(dplyr)
library(sf)
library(sp)
library(FRK)
library(reshape2)
library(Vizumap)

data(UB)
UB_dat <- read.uv(data = UB_tss, estimate = "TSS", error = "TSS_error")
sf_use_s2(FALSE)
UB_sf <- st_as_sf(UB_shp)
UB_shp <- st_as_sf(UB_shp)
UB_shp <- st_make_valid(UB_shp)
UB_shp <- sf::as_Spatial(UB_shp)


StatSPixelMap <- ggproto('StatSPixelMap', Stat,
                         required_aes = c('id', 'estimate', 'error'),

                         compute_panel = function(data, scales, coord,
                                                  id_col,
                                                  distribution,
                                                  pixelSize,
                                                  q,
                                                  sp_object) {

                           pixel_df <- pixelate(
                             geoData = sp_object,
                             pixelSize = pixelSize,
                             id = id_col
                           )

                           data$id <- as.character(data$id)
                           pixel_df[[id_col]] <- as.character(pixel_df[[id_col]])
                           pixel_df$estimate <- data$estimate[match(pixel_df[[id_col]], data$id)]
                           pixel_df$error <- data$error[match(pixel_df[[id_col]], data$id)]

                           if (distribution == "discrete") {
                             q2 <- as.data.frame(q)
                             q2[[id_col]] <- as.character(q2[[id_col]])
                             pixel_df$q <- split(q2[ , setdiff(names(q2), id_col)],
                                                 seq_len(nrow(q2)))[match(pixel_df[[id_col]], q2[[id_col]])]
                           }

                           createU <- function(idx, est, err) {
                             lo <- unique(est[idx] - err[idx])
                             hi <- unique(est[idx] + err[idx])
                             vec <- seq(lo, hi, length.out = 5)
                             sample(vec, length(idx), replace = TRUE)
                           }
                           createN <- function(idx, est, err) {
                             rnorm(length(idx), mean = unique(est[idx]), sd = unique(err[idx]))
                           }
                           createD <- function(idx, qrow) {
                             sample(unlist(qrow), length(idx), replace = TRUE)
                           }

                           if (distribution == "uniform") {
                             rvarray <- tapply(seq_len(nrow(pixel_df)),
                                               pixel_df$ID,
                                               FUN = createU,
                                               est = pixel_df$estimate,
                                               err = pixel_df$error)
                           } else if (distribution == "normal") {
                             rvarray <- tapply(seq_len(nrow(pixel_df)),
                                               pixel_df$ID,
                                               FUN = createN,
                                               est = pixel_df$estimate,
                                               err = pixel_df$error)
                           } else {
                             rvarray <- tapply(seq_len(nrow(pixel_df)),
                                               pixel_df$ID,
                                               FUN = createD,
                                               qrow = pixel_df$q)
                           }
                           rvmelt <- reshape2::melt(rvarray)
                           pixel_df$values <- unlist(rvmelt$value)

                           pixel_df
                         }
)

stat_spixel_map <- function(data, sp_object,
                            id_col = "id",
                            estimate = "estimate",
                            error = "error",
                            distribution = c("uniform","normal","discrete"),
                            pixelSize = 40,
                            q = NULL,
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {
  distribution <- match.arg(distribution)

  df_attr <- as.data.frame(data)[ , c(id_col, estimate, error), drop = FALSE ]
  names(df_attr) <- c("id", "estimate", "error")

  out_df <- StatSPixelMap$compute_panel(
    data = df_attr,
    scales = list(),
    coord = coord_sf(),
    id_col = id_col,
    distribution = distribution,
    pixelSize = pixelSize,
    q = q,
    sp_object = sp_object
  )

  layer(
    stat = 'identity',
    geom = 'polygon',
    data = out_df,
    mapping = aes(x = long, y = lat, group = group, fill = values),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot() +
  stat_spixel_map(
    data = UB_dat,
    sp_object = UB_shp,
    id_col = "scID",
    estimate = "TSS",
    error = "TSS_error",
    distribution = "uniform",
    pixelSize = 50
  ) +
  geom_sf(
    data = UB_sf,
    fill = NA,
    color = "black",
    size = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  coord_sf() +
  theme_void()

