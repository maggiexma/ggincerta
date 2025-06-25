library(Vizumap)
library(ggplot2)
library(dplyr)
library(spbabel)
library(plyr)

data(UB) # this returns a data frame, UB_tss, and a shapefile, UB_shp
UB_dat <- read.uv(data = UB_tss, estimate = "TSS", error = "TSS_error")

StatSPBivariate <- ggproto('StatSPBivariate', Stat,
                           required_aes = c('id', 'estimate', 'error'),
                           compute_panel = function(data, scales, terciles, bound, flipAxis, sp_object) {

                             x <- data$estimate; y <- data$error
                             if (is.null(bound)) {

                               if (terciles) {
                                 x_breaks <- unique(quantile(x, probs = c(0,1/3,2/3,1), na.rm=TRUE))
                                 y_breaks <- unique(quantile(y, probs = c(0,1/3,2/3,1), na.rm=TRUE))
                               } else {
                                 x_breaks <- seq(min(x,na.rm=TRUE), max(x,na.rm=TRUE), length.out=4)
                                 y_breaks <- seq(min(y,na.rm=TRUE), max(y,na.rm=TRUE), length.out=4)
                               }
                             }

                             if (!flipAxis) {
                               est_bin <- cut(x, breaks = x_breaks,
                                              include.lowest = TRUE,
                                              labels = FALSE)
                               err_bin <- cut(y, breaks = y_breaks,
                                              include.lowest = TRUE,
                                              labels = FALSE)
                             } else {
                               est_bin <- cut(y, breaks = y_breaks,
                                              include.lowest = TRUE,
                                              labels = FALSE)
                               err_bin <- cut(x, breaks = x_breaks,
                                              include.lowest = TRUE,
                                              labels = FALSE)
                             }

                             n_err <- max(err_bin, na.rm = TRUE)
                             combo_num <- (est_bin - 1) * n_err + err_bin

                             data$fill <- factor(combo_num,
                                                 levels = seq_len(n_err * n_err))

                             sp_object@data %>%
                               mutate_if(is.factor, as.character) -> sp_object@data

                             sp_object@data <- left_join(sp_object@data, data, by = 'id')

                             sp_object@data$id <- rownames(sp_object@data)

                             region_coord <- sptable(sp_object, region = 'id')
                             region_coord <- plyr::rename(region_coord, c(
                               'object_' = 'id', 'x_' = 'long', 'y_' = 'lat', 'branch_' = 'group'
                             ))

                             output_data <- plyr::join(region_coord, sp_object@data, by = 'id')

                             output_data
                           }
)

stat_sp_bivariate <- function(data = NULL, sp_object = NULL,
                              id_col = 'id', estimate = 'estimate', error = 'error',
                              terciles = FALSE, bound = NULL, flipAxis = FALSE,
                              position = 'identity', na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {

  df_in <- as.data.frame(data)[, c(id_col, estimate, error)]
  names(df_in) <- c('id', 'estimate', 'error')

  sp_copy <- sp_object
  sp_copy@data$id <- sp_copy@data[[ id_col ]]

  out_df <- StatSPBivariate$compute_panel(
    data = df_in,
    scales = list(),
    terciles = terciles,
    bound = bound,
    flipAxis = flipAxis,
    sp_object = sp_copy
  )

  layer(
    stat = 'identity',
    geom = 'polygon',
    data = out_df,
    mapping = aes(x = long, y = lat, group = group, fill = fill),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot() +
  stat_sp_bivariate(
    data = UB_dat,
    sp_object = UB_shp,
    id_col = 'scID',
    estimate = 'TSS',
    error = 'TSS_error',
    terciles = TRUE
  ) +
  geom_sf(
    data = UB_sf,
    fill = NA,
    color = "black",
    size = 0.3,
    inherit.aes = FALSE
  ) +
  scale_fill_bivar(colrange = list(colour = c('gold','red4'), difC = c(4,4))) +
  coord_sf() +
  theme_void()
