library(ggplot2)
library(grid)
library(legendry)

scale_fill_bivariate <- function(colrange,
                                 subtractive = FALSE,
                                 flip_vertical = FALSE,
                                 flip_horizontal = FALSE,
                                 size = 3,
                                 ...) {
  grad1 <- grDevices::colorRampPalette(c("white", colrange$colour[1]))
  grad2 <- grDevices::colorRampPalette(c("white", colrange$colour[2]))
  dif1 <- rev(grad1(10)[1:4]); dif2 <- rev(grad2(10)[1:4])
  ramp1 <- grDevices::colorRamp(c(dif1[colrange$difC[1]], colrange$colour[1]))
  ramp2 <- grDevices::colorRamp(c(dif2[colrange$difC[2]], colrange$colour[2]))
  lam1 <- c(0,.5,1,0,.5,1,0,.5,1); lam2 <- c(0,0,0,.5,.5,.5,1,1,1)
  m1 <- ramp1(lam1); m2 <- ramp2(lam2)
  if (subtractive) {
    m1 <- PBSmapping::RGB2RYB(m1); m2 <- PBSmapping::RGB2RYB(m2)
    m1[is.na(m1)] <- 0; m2[is.na(m2)] <- 0
    mix <- round(PBSmapping::RYB2RGB((m1 + m2) / 2) * 255)
  } else {
    mix <- round((m1 + m2) / 2)
  }
  cols <- apply(mix, 1, function(v) grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255))
  if (flip_vertical)   cols <- cols[c(9,4,5,2,3,6,1,8,7)]
  if (flip_horizontal) cols <- cols[c(7,8,9,4,5,6,1,2,3)]

  df <- expand.grid(r = 1:3, c = 1:3)
  tiles <- lapply(seq_len(9), function(i) {
    grid::rectGrob(
      x = unit((df$c[i] - .5)/3, "npc"),
      y = unit((df$r[i] - .5)/3, "npc"),
      width = unit(1/3, "npc"),
      height = unit(1/3, "npc"),
      gp = gpar(fill = cols[i], col = "black")
    )
  })
  offset <- size * (sqrt(2)/2 - 1/2)

  bivar_grob <- gTree(
    children = do.call(gList, tiles),
    vp = viewport(
      x      = unit(0.5, "npc") - unit(offset, "cm"),
      y      = unit(0.5, "npc"),
      width  = unit(size * sqrt(2), "cm"),
      height = unit(size * sqrt(2), "cm"),
      angle  = 45
    )
  )

  bivar_gizmo <- gizmo_grob(
    bivar_grob,
    width  = unit(size * sqrt(2), "cm"),
    height = unit(size * sqrt(2), "cm")
  )

  ggplot2::scale_fill_manual(
    values = cols,
    drop = FALSE,
    guide = bivar_gizmo,
    ...
  )
}

ggplot(nc) +
  stat_sf_bivariate(aes(estimate = value, error = sd, fill = after_stat(fill)),
                    terciles = TRUE) +
  scale_fill_bivariate(
    colrange = list(colour = c("blue","red4"), difC = c(4,4)),
    size = 2
  ) +
  theme_minimal()

