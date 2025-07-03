library(ggplot2)
library(grid)

GuideBivariate <- ggproto("GuideBivariate", GuideLegend,

                          params = c(
                            GuideLegend$params,
                            list(colors = NULL)
                          ),

                          train = function(self, scale, aesthetic = NULL, ...) {
                            self$params$colors$value <- scale$range$range
                            self$params$title$value  <- scale$name %||% aesthetic
                            self
                          },

                          draw_key = function(data, params, size) {
                            rectGrob(gp = gpar(fill = data$fill, col = "black"))
                          },

                          draw = function(self, theme, position = NULL, direction = NULL, params = self$params) {
                            cols <- params$colors$value
                            df   <- expand.grid(row = 1:3, col = 1:3)
                            tiles <- lapply(seq_len(9), function(i) {
                              rectGrob(
                                x      = unit((df$col[i] - .5)/3, "npc"),
                                y      = unit((df$row[i] - .5)/3, "npc"),
                                width  = unit(1/3, "npc"),
                                height = unit(1/3, "npc"),
                                gp     = gpar(fill = cols[i], col = "black")
                              )
                            })
                            title_grob <- textGrob(
                              label = params$title$value,
                              x     = unit(.5, "npc"),
                              y     = unit(1,   "npc") + unit(1, "mm"),
                              just  = "bottom",
                              gp    = gpar(fontsize = theme$text$size)
                            )
                            gTree(children = do.call(gList, c(list(title_grob), tiles)))
                          }
)

guide_bivariate <- function(colors,
                            title = "Bivariate key",
                            order = 0,
                            position = "right",
                            direction = "vertical",
                            theme = theme_get(),
                            hash = NULL) {
  if (is.null(hash)) {
    hash <- paste0("biv-", paste0(sub("#","", colors), collapse = "-"))
  }
  ggplot2::new_guide(
    name = "bivariate",
    title = title,
    order = order,
    position = position,
    direction = direction,
    theme = theme,
    hash = hash,
    available_aes = "fill",
    colors = colors,
    super = GuideBivariate
  )
}

scale_fill_bivariate <- function(colrange,
                                 subtractive     = FALSE,
                                 flip_vertical   = FALSE,
                                 flip_horizontal = FALSE,
                                 ...) {

  grad1 <- grDevices::colorRampPalette(c("white", colrange$colour[1]))
  grad2 <- grDevices::colorRampPalette(c("white", colrange$colour[2]))
  dif1 <- rev(grad1(10)[1:4]); dif2 <- rev(grad2(10)[1:4])
  ramp1 <- grDevices::colorRamp(c(dif1[colrange$difC[1]], colrange$colour[1]))
  ramp2 <- grDevices::colorRamp(c(dif2[colrange$difC[2]], colrange$colour[2]))
  lam1 <- c(0,.5,1,0,.5,1,0,.5,1); lam2 <- c(0,0,0,.5,.5,.5,1,1,1)
  m1   <- ramp1(lam1); m2   <- ramp2(lam2)
  if (subtractive) {
    m1 <- PBSmapping::RGB2RYB(m1); m2 <- PBSmapping::RGB2RYB(m2)
    m1[is.na(m1)] <- 0; m2[is.na(m2)] <- 0
    mix <- round(PBSmapping::RYB2RGB((m1 + m2) / 2) * 255)
  } else {
    mix <- round((m1 + m2) / 2)
  }
  cols <- apply(mix, 1, function(v) {
    grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
  })
  if (flip_vertical)   cols <- cols[c(9,4,5,2,3,6,1,8,7)]
  if (flip_horizontal) cols <- cols[c(7,8,9,4,5,6,1,2,3)]

  ggplot2::scale_fill_manual(
    values = cols,
    drop   = FALSE,
    guide  = guide_bivariate(colors = cols),
    ...
  )
}

ggplot(nc) +
  stat_sf_bivariate(
    aes(estimate = value, error = sd, fill = after_stat(fill)),
    terciles = TRUE
  ) +
  scale_fill_bivariate(
    colrange = list(colour = c("gold", "red4"), difC = c(4,4))
  ) +
  theme_minimal()
