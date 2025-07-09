library(ggplot2)

StatGKey <- ggproto("StatGKey", Stat,
                    required_aes = "error",
                    compute_panel = function(data, scales, glyph, fontSize) {
                      if (! glyph %in% c("icone","semi"))
                        stop("Glyph name not recognised. Must be one of icone or semi.")
                      if (glyph == "icone") {
                        x0 <- seq(-3, 3, 0.05); y0 <- sqrt(9 - x0^2) + 5
                        cir <- data.frame(V1 = x0, V2 = y0)
                        tri <- data.frame(V1 = c(-3, 0, 3), V2 = c(5, 0, 5))
                        base <- rbind(cir, tri)
                      } else {
                        x0 <- seq(-3, 3, 0.05); y0 <- sqrt(9 - x0^2) + 5
                        base <- data.frame(V1 = x0, V2 = y0)
                      }
                      
                      errs <- data$error
                      max_err <- max(errs, na.rm=TRUE)
                      mids <- max_err/2
                      
                      make_polys <- function(theta) {
                        R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2)
                        N <- as.data.frame(t(R %*% t(base)))
                        names(N) <- c("V1","V2")
                        N$id <- theta
                        N$type <- "poly"
                        N$label <- NA
                        N$size <- NA
                        N
                      }
                      
                      angles3 <- -(c(0, mids, max_err) / max_err) * pi
                      main3 <- do.call(rbind, lapply(angles3, make_polys))
                      main3$main <- TRUE
                      
                      angles5 <- -(seq(0, max_err, length.out=6) / max_err) * pi
                      extra5<- do.call(rbind, lapply(angles5, make_polys))
                      extra5$main <- FALSE
                      
                      labels <- data.frame(V1 = c(-4, 11.1, -5.5),
                                           V2 = c(7, 1.4, -7), id = angles3,
                                           type = "label", label = round(c(0, mids, max_err), 2),
                                           size = fontSize, stringsAsFactors = FALSE)
                      labels$main <- NA
                      rbind(main3, extra5, labels)
                    }
)

geom_glyph_key <- function(data, mapping = aes(error = error), 
                           glyph = "icone", fontSize = 3) {
  err_col <- rlang::as_name(rlang::quo_get_expr(mapping$error))
  df_in <- data.frame(error = data[[err_col]])
  df_out <- StatGKey$compute_panel(data = df_in, scales = list(),
                                   glyph = glyph, fontSize = fontSize)

  extra5 <- df_out[df_out$main == FALSE, ]
  main3 <- df_out[df_out$main == TRUE,  ]

  max_err <- max(df_in$error, na.rm = TRUE)
  mids <- max_err / 2
  main3_labels <- round(c(0, mids, max_err), 2)
  key_label <- err_col

  list(
    geom_polygon(data = extra5, mapping = aes(x = V1, y = V2, group = id), 
                 fill = "black", alpha = 0.2, inherit.aes = FALSE),

    geom_polygon(data = main3, mapping = aes(x = V1, y = V2, group = id),
                 fill = "black", alpha = 0.7, inherit.aes = FALSE),

    geom_segment(data = data.frame(x = -5, xend = 10, y = 0, yend = 0),
                 mapping = aes(x = x, xend = xend, y = y, yend = yend),
                 colour = "black", alpha = 0.5, inherit.aes = FALSE),

    geom_segment(data = data.frame(x = 0, xend = 0, y = 10.5, yend = -10.5),
                 mapping = aes(x = x, xend = xend, y = y, yend = yend),
                 colour = "black", alpha = 0.5, inherit.aes = FALSE),

    geom_text(data = data.frame(x = 0, y = 12, label = key_label), 
              mapping = aes(x = x, y = y, label = label), 
              size = fontSize, inherit.aes = FALSE),

    geom_text(data = data.frame(x = -4, y = 7, label = main3_labels[1]),
              mapping = aes(x = x, y = y, label = label), 
              size = fontSize, inherit.aes = FALSE),
    
    geom_text(data = data.frame(x = 11.1, y = 1.4, label = main3_labels[2]),
              mapping = aes(x = x, y = y, label = label),
              size = fontSize, inherit.aes = FALSE),
    
    geom_text(data = data.frame(x = -5.5, y = -7, label = main3_labels[3]),
              mapping = aes(x = x, y = y, label = label), 
              size = fontSize, inherit.aes = FALSE
    )
  )
}

g_map <- ggplot(nc, aes(geometry = geometry, estimate = value, error = sd)) +
  stat_sf_glyph(glyph = "icone") +
  coord_sf() + theme_void()

g_key <- ggplot() + 
  geom_glyph_key(data = nc, mapping = aes(error = sd), glyph = "icone", 
                 fontSize = 3) +
  coord_equal() + theme_void()
g_map + g_key + plot_layout(widths = c(3, 1))

