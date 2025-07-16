scale_fill_bivariate <- function(data,
                                 estimate = "estimate",
                                 error = "error",
                                 colrange = list(colour = c("gold", "red4"), difC = c(4, 4)),
                                 subtractive = FALSE,
                                 flip_vertical = FALSE,
                                 flip_horizontal = FALSE,
                                 ...) {

  est <- data[[estimate]]
  err <- data[[error]]

  est_breaks <- round(quantile(est, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 2)
  err_breaks <- round(quantile(err, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 2)

  grad1 <- grDevices::colorRampPalette(c("white", colrange$colour[1]))
  grad2 <- grDevices::colorRampPalette(c("white", colrange$colour[2]))

  dif1 <- rev(grad1(10)[1:4])
  dif2 <- rev(grad2(10)[1:4])
  ramp1 <- grDevices::colorRamp(c(dif1[colrange$difC[1]], colrange$colour[1]))
  ramp2 <- grDevices::colorRamp(c(dif2[colrange$difC[2]], colrange$colour[2]))

  lam1 <- c(0, .5, 1, 0, .5, 1, 0, .5, 1)
  lam2 <- c(0, 0, 0, .5, .5, .5, 1, 1, 1)

  m1 <- ramp1(lam1)
  m2 <- ramp2(lam2)

  if (subtractive) {
    m1 <- PBSmapping::RGB2RYB(m1)
    m2 <- PBSmapping::RGB2RYB(m2)
    m1[is.na(m1)] <- 0
    m2[is.na(m2)] <- 0
    mix <- round(PBSmapping::RYB2RGB((m1 + m2) / 2) * 255)
  } else {
    mix <- round((m1 + m2) / 2)
  }

  cols <- apply(mix, 1, function(v) grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255))
  if (flip_vertical)   cols <- cols[c(9, 4, 5, 2, 3, 6, 1, 8, 7)]
  if (flip_horizontal) cols <- cols[c(7, 8, 9, 4, 5, 6, 1, 2, 3)]

  scale_fill_manual(
    values = cols,
    drop = FALSE,
    guide = guide_bivariate(
      aesthetic = cols,
      value = as.character(1:9),
      label = as.character(1:9),
      est_label = est_breaks,
      err_label = err_breaks
    ),
    ...
  )
}

draw_bivariate_key <- function(key, size = 2, est_label = NULL, err_label = NULL) {
  layout <- expand.grid(row = 1:3, col = 1:3)

  tiles <- lapply(seq_len(9), function(i) {
    grid::rectGrob(
      x = unit((layout$col[i] - 0.5) / 3, "npc"),
      y = unit((layout$row[i] - 0.5) / 3, "npc"),
      width  = unit(1/3, "npc"),
      height = unit(1/3, "npc"),
      gp = grid::gpar(fill = key$fill[i], col = "black")
    )
  })

  grobs <- tiles

  offset_x <- 0.07
  offset_y <- -0.02

  if (!is.null(est_label)) {
    for (i in 1:4) {
      grobs[[length(grobs) + 1]] <- grid::textGrob(
        label = est_label[i],
        x = unit(c(0, 1/3, 2/3, 1)[i], "npc") + unit(offset_x, "npc"),
        y = unit(-0.18, "npc") + unit(offset_y, "npc"),
        just = "top",
        rot = -90,
        gp = grid::gpar(fontsize = 8)
      )
    }
  }

  if (!is.null(err_label)) {
    for (i in 1:4) {
      grobs[[length(grobs) + 1]] <- grid::textGrob(
        label = err_label[i],
        x = unit(-0.05, "npc"),
        y = unit(c(0, 1/3, 2/3, 1)[i], "npc"),
        just = "right",
        gp = grid::gpar(fontsize = 8)
      )
    }
  }

  rotated_grob <- grid::grobTree(
    children = do.call(grid::gList, grobs),
    vp = grid::viewport(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      width = unit(1, "npc"),
      height = unit(1, "npc"),
      angle = 45
    )
  )

  width <- grid::unit(size, "cm")
  height <- grid::unit(size, "cm")

  # width <- grid::unit(size, "npc")
  # height <- grid::unit(size, "npc")

  gt <- gtable::gtable(widths = width, heights = height)
  gt <- gtable::gtable_add_grob(gt, rotated_grob, t = 1, l = 1, clip = "off", name = "bivar_key")

  return(gt)
}

GuideBivariate <- ggproto(
  "GuideBivariate",
  GuideLegend,
  params = c(GuideLegend$params, list(
    key = NULL,
    est_label = NULL,
    err_label = NULL
  )),

  draw = function(self, theme, params = self$params, ...) {
    draw_bivariate_key(
      key = params$key,
      size = 2,
      est_label = params$est_label,
      err_label = params$err_label
    )
  }
)

guide_bivariate <- function(aesthetic,
                            value,
                            label,
                            est_label = NULL,
                            err_label = NULL,
                            ...,
                            theme = NULL,
                            title = waiver(),
                            order = 0,
                            position = NULL) {
  key <- data.frame(
    aesthetic,
    .value = value,
    .label = label
  )

  new_guide(
    key = key,
    est_label = est_label,
    err_label = err_label,
    theme = theme,
    title = title,
    order = order,
    position = position,
    available_aes = 'fill',
    super = GuideBivariate
  )
}

# ggplot(nc) +
#   geom_sf_bivariate(aes(geometry = geometry, estimate = value, error = sd)) +
#   scale_fill_bivariate(
#     data = nc,
#     estimate = "value",
#     error = "sd",
#     colrange = list(colour = c("gold", "red4"), difC = c(4, 4))
#   )

ggplot(nc) +
  geom_sf_bivariate(aes(geometry = geometry, estimate = value, error = sd)) +
  scale_fill_bivariate(
    data = nc,
    estimate = "value",
    error = "sd",
    colrange = list(colour = c("gold", "red4"), difC = c(4, 4))
  ) +
  theme(
    legend.position = "right",
    legend.box.just = "left",
    plot.margin = margin(10, 30, 10, 10)
  )
