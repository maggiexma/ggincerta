library(ggplot2)
library(grid)

StatBKey <- ggproto("StatBKey", Stat,
                    required_aes = c("estimate","error"),
                    compute_panel = function(data, scales, terciles, flipAxis,
                                             bound, expertR_est, expertR_err) {

                      if (is.null(bound)) {
                        x <- data$estimate
                        y <- data$error

                        x_min <- min(c(expertR_est, x), na.rm = TRUE)
                        x_max <- max(c(expertR_est, x), na.rm = TRUE)
                        y_min <- min(c(expertR_err, y), na.rm = TRUE)
                        y_max <- max(c(expertR_err, y), na.rm = TRUE)

                        if (terciles) {
                          x_breaks <- as.numeric(quantile(x, probs = c(0,1/3,2/3,1), na.rm = TRUE))
                          y_breaks <- as.numeric(quantile(y, probs = c(0,1/3,2/3,1), na.rm = TRUE))
                        } else {
                          x_breaks <- seq(x_min, x_max, length.out = 4)
                          y_breaks <- seq(y_min, y_max, length.out = 4)
                        }
                        bound <- c(x_breaks, y_breaks)
                      }

                      x1 <- c(3,4,3,2); x2 <- c(4,5,4,3); x3 <- c(5,6,5,4)
                      xs <- c(x1, x1-1, x1-2, x2, x2-1, x2-2, x3, x3-1, x3-2)
                      y1 <- c(0,1,2,1); y2 <- c(1,2,3,2); y3 <- c(2,3,4,3)
                      ys <- c(y1, y1+1, y1+2, y2, y2+1, y2+2, y3, y3+1, y3+2)

                      tiles <- data.frame(x = xs, y = ys, group = rep(1:9, each = 4),
                                          fill  = factor(rep(1:9, each = 4)),
                                          bound = NA, angle = NA, type  = "tile")

                      if (!flipAxis) {
                        lab <- data.frame(x = c(2.5,1.5,0.5,-0.5, 3.5,4.5,5.5,6.5),
                                          y = c(-0.5,0.5,1.5,2.5,-0.5,0.5,1.5,2.5),
                                          group = NA, fill = NA, bound = round(bound,2),
                                          angle = c(rep(45,4), rep(-45,4)),
                                          type  = "label")
                      } else {
                        lab <- data.frame(x = c(-0.5,0.5,1.5,2.5,-0.5,0.5,1.5,2.5),
                                          y = c(2.5,1.5,0.5,-0.5,3.5,4.5,5.5,6.5),
                                          group = NA, fill = NA, bound = round(bound,2),
                                          angle = c(rep(45,4), rep(-45,4)),
                                          type  = "label")
                      }
                      rbind(tiles, lab)
                    }
)

stat_bkey <- function(mapping = NULL, data = NULL, geom = "polygon",
                      position = "identity", na.rm = FALSE, show.legend = FALSE,
                      inherit.aes = TRUE, terciles = TRUE, flipAxis = FALSE,
                      bound = NULL, expertR_est = NA, expertR_err = NA, ...) {
  layer(
    stat = StatBKey,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      terciles = terciles,
      flipAxis = flipAxis,
      bound = bound,
      expertR_est = expertR_est,
      expertR_err = expertR_err,
      na.rm = na.rm,
      ...
    )
  )
}

ggplot(nc, aes(estimate = value, error = sd)) +
  stat_bkey(aes(fill = after_stat(fill), group = after_stat(fill)),
            colour = 'black', linewidth = 0.5) +
  scale_fill_bivar(colrange = list(colour = c("gold","red4"), difC = c(4,4)),
                   subtractive  = FALSE, flip_vertical = FALSE,
                   flip_horizontal = FALSE) + coord_equal() + theme_void()
