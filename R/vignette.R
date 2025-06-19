StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  stat_chull(fill = NA, colour = "black")

ggplot(mpg, aes(displ, hwy, colour = drv)) + 
  geom_point() +
  stat_chull(fill = NA)

ggplot(mpg, aes(displ, hwy)) + 
  stat_chull(geom = "point", size = 4, colour = "red") +
  geom_point()

StatLm <- ggproto("StatLm", Stat, 
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales) {
                    browser()
                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = rng)
                    
                    mod <- lm(y ~ x, data = data)
                    grid$y <- predict(mod, newdata = grid)
                    
                    grid
                  }
)

stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA, 
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatLm, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  stat_lm()

StatLm <- ggproto("StatLm", Stat, 
                  required_aes = c("x", "y"),
                  
                  compute_group = function(data, scales, params, n = 100, formula = y ~ x) {
                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = seq(rng[1], rng[2], length = n))
                    
                    mod <- lm(formula, data = data)
                    grid$y <- predict(mod, newdata = grid)
                    
                    grid
                  }
)

stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA, 
                    inherit.aes = TRUE, n = 50, formula = y ~ x, 
                    ...) {
  layer(
    stat = StatLm, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, formula = formula, na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  stat_lm(formula = y ~ poly(x, 10)) + 
  stat_lm(formula = y ~ poly(x, 10), geom = "point", colour = "red", n = 20)

StatDensityCommon <- ggproto("StatDensityCommon", Stat, 
                             required_aes = "x",
                             
                             setup_params = function(data, params) {
                               if (!is.null(params$bandwidth))
                                 return(params)
                               
                               xs <- split(data$x, data$group)
                               bws <- vapply(xs, bw.nrd0, numeric(1))
                               bw <- mean(bws)
                               message("Picking bandwidth of ", signif(bw, 3))
                               
                               params$bandwidth <- bw
                               params
                             },
                             
                             compute_group = function(data, scales, bandwidth = 1) {
                               d <- density(data$x, bw = bandwidth)
                               data.frame(x = d$x, y = d$y)
                             }  
)

stat_density_common <- function(mapping = NULL, data = NULL, geom = "line",
                                position = "identity", na.rm = FALSE, show.legend = NA, 
                                inherit.aes = TRUE, bandwidth = NULL,
                                ...) {
  layer(
    stat = StatDensityCommon, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(bandwidth = bandwidth, na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, colour = drv)) + 
  stat_density_common()

ggplot(mpg, aes(displ, colour = drv)) + 
  stat_density_common(bandwidth = 0.5)

StatDensityCommon <- ggproto("StatDensity2", Stat, 
                             required_aes = "x",
                             default_aes = aes(y = after_stat(density)),
                             
                             compute_group = function(data, scales, bandwidth = 1) {
                               d <- density(data$x, bw = bandwidth)
                               data.frame(x = d$x, density = d$y)
                             }  
)

ggplot(mpg, aes(displ, drv, colour = after_stat(density))) + 
  stat_density_common(bandwidth = 1, geom = "point")

ggplot(mpg, aes(displ, fill = drv)) + 
  stat_density_common(bandwidth = 1, geom = "area", position = "stack")

StatDensityCommon <- ggproto("StatDensityCommon", Stat, 
                             required_aes = "x",
                             default_aes = aes(y = after_stat(density)),
                             
                             setup_params = function(data, params) {
                               min <- min(data$x) - 3 * params$bandwidth
                               max <- max(data$x) + 3 * params$bandwidth
                               
                               list(
                                 bandwidth = params$bandwidth,
                                 min = min,
                                 max = max,
                                 na.rm = params$na.rm
                               )
                             },
                             
                             compute_group = function(data, scales, min, max, bandwidth = 1) {
                               d <- density(data$x, bw = bandwidth, from = min, to = max)
                               data.frame(x = d$x, density = d$y)
                             }  
)

ggplot(mpg, aes(displ, fill = drv)) + 
  stat_density_common(bandwidth = 1, geom = "area", position = "stack")

ggplot(mpg, aes(displ, drv, fill = after_stat(density))) + 
  stat_density_common(bandwidth = 1, geom = "raster")

