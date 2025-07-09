library(ggplot2)
library(sp)
library(dplyr)

StatSpGlyph <- ggproto('StatSpGlyph', Stat,
                       required_aes = c('id', 'estimate', 'error'),

                       compute_panel = function(data, scales,
                                                size = 90,
                                                glyph = 'icone',
                                                max_error = NULL,
                                                sp_object) {

                         coords <- sp::coordinates(sp_object)
                         df <- sp_object@data

                         df$id <- as.character(df$id)
                         data$id <- as.character(data$id)
                         df$estimate <- data$estimate[match(df$id, data$id)]
                         df$error <- data$error[match(df$id, data$id)]

                         df$long <- coords[,1]
                         df$lat <- coords[,2]
                         errs <- df$error
                         mx <- if (is.null(max_error)) max(errs, na.rm=TRUE) else max_error
                         df$theta <- -(errs / mx) * pi
                         df$size <- size
                         df$glyph <- glyph

                         polys <- lapply(seq_len(nrow(df)), function(i) {

                           if (df$glyph[i] == 'icone') {
                             x1 <- seq(-3, 3, .05); y1 <- sqrt(9 - x1^2)
                             cir <- data.frame(x = x1, y = y1)
                             tri <- data.frame(x = c(-3, 0, 3), y = c(0, -5, 0))
                             base <- rbind(cir, tri)
                           } else if (df$glyph[i] == 'semi') {
                             x1 <- seq(-3, 3, .05); y1 <- sqrt(9 - x1^2)
                             base <- data.frame(x = x1, y = y1)
                           } else {
                             stop("Glyph must be 'icone' or 'semi'")
                           }

                           th <- df$theta[i]
                           R <- matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2)
                           M <- (R %*% t(base / df$size[i]))
                           M <- as.data.frame(t(M))

                           data.frame(
                             val = df$estimate[i],
                             err = df$error[i],
                             id = df$id[i],
                             long = M$V1 + df$long[i],
                             lat = M$V2 + df$lat[i],
                             stringsAsFactors = FALSE
                           )
                         })

                         do.call(rbind, polys)
                       }
)

stat_sp_glyph <- function(data, sp_object,
                          id_col = 'id',
                          estimate = 'estimate',
                          error = 'error',
                          size = 50,
                          glyph = 'icone',
                          max_error = NULL,
                          position = 'identity',
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {

  df_attr <- as.data.frame(data)[, c(id_col, estimate, error), drop = FALSE]
  names(df_attr) <- c('id', 'estimate', 'error')

  sp_copy <- sp_object
  sp_copy@data$id <- sp_copy@data[[ id_col ]]

  out_df <- StatSpGlyph$compute_panel(
    data      = df_attr,
    scales    = list(),
    size      = size,
    glyph     = glyph,
    max_error = max_error,
    sp_object = sp_copy
  )

  layer(
    stat = 'identity',
    geom = 'polygon',
    data = out_df,
    mapping = aes(x = long, y = lat, group = id, fill = val),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot() +
  stat_sp_glyph(
    data = UB_dat,
    sp_object = UB_shp,
    id_col = 'scID',
    estimate = 'TSS',
    error = 'TSS_error',
    size = 90,
    glyph = 'icone'
  ) +
  scale_fill_distiller(palette = 'Blues') +
  coord_equal() +
  theme_void()
