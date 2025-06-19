library(ggplot2)
library(rlang)
library(patchwork)

geom_bivar_key <- function(mapping = NULL, data = NULL, terciles = TRUE, flipAxis = FALSE,
                           bound = NULL, expertR_est = NA, expertR_err = NA,
                           fontSize = 3, ...) {

  est_name <- as_label(mapping$estimate)
  err_name <- as_label(mapping$error)
  
  list(
    stat_bkey(
      mapping = aes(fill = after_stat(fill), group = after_stat(fill)), 
      data = data, geom = "polygon", inherit.aes = TRUE, show.legend = FALSE,
      colour = 'black', linewidth = 0.5, 
      terciles = terciles, flipAxis = flipAxis, bound = bound,
      expertR_est = expertR_est, expertR_err = expertR_err, ...),
    
    stat_bkey(
      mapping = aes(x = after_stat(x), y = after_stat(y), 
                    label = after_stat(bound), angle = after_stat(angle)),
      data = data, geom = "text", inherit.aes = TRUE, show.legend = FALSE,
      terciles = terciles, flipAxis = flipAxis, bound = bound,
      expertR_est = expertR_est, expertR_err = expertR_err, na.rm = TRUE,
      size = fontSize, ...),

    annotate("text", x = -0.5, y = 0.1, label = est_name, 
             angle = -45, size  = fontSize),
    annotate("text", x = 6.5, y = 0.1, label = err_name, 
             angle = 45, size = fontSize),
    
    coord_equal(),
    theme_void()
  )
}

ggplot(nc, aes(estimate = value, error = sd)) +
  geom_bivar_key(mapping = aes(estimate = value, error = sd), 
                 terciles = TRUE, expertR_est = NA, expertR_err = NA, 
                 bound = NULL, fontSize = 3) +
  scale_fill_bivar(colrange = list(colour=c("gold","red4"), difC=c(4,4)), 
                   subtractive = FALSE, flipVertical = FALSE, 
                   flipHorizontal = FALSE)

map_p <- ggplot(nc, aes(estimate = value, error = sd)) +
  stat_sf_bivariate(mapping = aes(fill = after_stat(fill)), terciles = TRUE) +
  scale_fill_bivar(colrange = list(colour = c("gold","red4"), difC = c(4,4)),
                   subtractive = FALSE, flipVertical = FALSE, 
                   flipHorizontal = FALSE) +
  coord_sf() +
  theme_void()

key_p <- ggplot(nc, aes(estimate = value, error = sd)) +
  geom_bivar_key(mapping = aes(estimate = value, error = sd), 
                 terciles = TRUE, fontSize = 3) +
  scale_fill_bivar(colrange = list(colour = c("gold","red4"), difC = c(4,4)),
                   subtractive = FALSE, flipVertical = FALSE, 
                   flipHorizontal = FALSE) +
  coord_equal() +
  theme_void()

map_p + key_p + plot_layout(widths = c(3, 1))
