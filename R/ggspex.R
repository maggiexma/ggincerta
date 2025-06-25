library(ggplot2)
library(dplyr)
library(spbabel)
library(plyr)

StatSpExceed <- ggproto("StatSpExceed", Stat,
                        required_aes = c("id","estimate","error"),
                        compute_panel = function(data, scales, coord,
                                                 dist_fun, threshold, sp_object) {

                          data$id <- as.character(data$id)
                          sp_object@data <- mutate_if(sp_object@data, is.factor, as.character)
                          sp_object@data$id <- as.character(sp_object@data[["scID"]])

                          sdf <- left_join(sp_object@data, data, by = "id")
                          sdf$id <- rownames(sdf)

                          region_coord <- sptable(sp_object, region = "id")
                          region_coord <- plyr::rename(region_coord, c(
                            object_ = "id", x_ = "long", y_ = "lat", branch_ = "group"
                          ))
                          out <- plyr::join(region_coord, sdf, by = "id")

                          if (!"pr_exc" %in% names(out)) {
                            out$pr_exc <- dist_fun(
                              q = threshold,
                              mean = out$estimate,
                              sd = out$error,
                              lower.tail = FALSE
                            )
                          }

                          out
                        }
)

stat_sp_exceed <- function(data, sp_object,
                           id_col = "scID",
                           dist_fun = stats::pnorm,
                           threshold,
                           palette = "YlOrRd",
                           direction = 1,
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {

  df_in <- as.data.frame(data)[, c(id_col, names(data)[1], names(data)[2]), drop = FALSE]
  names(df_in) <- c("id", "estimate", "error")

  out_df <- StatSpExceed$compute_panel(
    data = df_in,
    scales = list(),
    dist_fun = dist_fun,
    threshold = threshold,
    sp_object = sp_object,
    ...
  )

  layer_obj <- layer(
    stat = "identity",
    geom = "polygon",
    data = out_df,
    mapping = aes(x = long, y = lat, group = group, fill = pr_exc),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )

  legend_title <- paste0("Pr[X > ", threshold, "]")
  scale_obj <- scale_fill_distiller(
    palette = palette,
    direction = direction,
    name = legend_title
  )

  list(layer_obj, scale_obj)
}

ggplot() +
  stat_sp_exceed(
    data = UB_dat,
    sp_object = UB_shp,
    id_col = "scID",
    dist_fun = stats::pnorm,
    threshold = 837
  ) +
  geom_sf(
    data = UB_sf,
    fill = NA,
    color = "black",
    size = 0.3,
    inherit.aes = FALSE
  ) +
  coord_sf() +
  theme_void()
