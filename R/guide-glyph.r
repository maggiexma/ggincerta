GuideGlyph <- ggproto(
  "GuideGlyph",
  GuideLegend,
  params = c(
    GuideLegend$params,
    list(
      breaks = NULL,
      glyph = NULL,
      label = NULL,
      font_size = NULL
    )
  ),
  draw = function(self, theme, params = self$params, ...) {
    draw_glyph_key(
      key = params$breaks,
      glyph = params$glyph,
      label = params$label,
      font_size = params$font_size
    )
  }
)

guide_glyph <- function(breaks,
                        glyph = c("icone", "semi"),
                        label = NULL,
                        font_size = NULL,
                        theme = NULL,
                        title = waiver(),
                        order = 99,
                        position = NULL) {
  glyph <- match.arg(glyph)
  new_guide(
    breaks = breaks,
    glyph = glyph,
    label = label,
    font_size = font_size,
    theme = theme,
    title = title,
    order = order,
    position = position,
    available_aes = "glyph",
    super = GuideGlyph
  )
}

draw_glyph_key <- function(key,
                           glyph = c("icone", "semi"),
                           label = NULL,
                           font_size = NULL,
                           minor_n = 6,
                           alpha_major = 0.70,
                           alpha_minor = 0.20,
                           axis_col = "black",
                           axis_alpha = 0.45,
                           title_lift = grid::unit(4, "mm"),
                           lab_nudge_top = c(0.0, 0.8),
                           lab_nudge_right = c(0.9, 0.0),
                           lab_nudge_bottom = c(0.0, 0.7)) {
  glyph <- match.arg(glyph)

  base_shape <- function(g) {
    x0 <- seq(-3, 3, by = 0.05)
    y0 <- sqrt(pmax(0, 9 - x0^2)) + 5
    cir <- data.frame(x = x0, y = y0)
    if (g == "icone") {
      rbind(cir, data.frame(x = c(-3, 0, 3), y = c(5, 0, 5)))
    } else {
      cir
    }
  }
  base_xy <- base_shape(glyph)

  ang_major <- c(pi, -pi / 2, 0)
  ang_minor <- seq(0, -pi, length.out = minor_n)

  rot <- function(th)
    matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2)
  rotate_df <- function(xy, th)
    as.data.frame(t(rot(th) %*% t(as.matrix(xy))))

  make_polys <- function(thetas, a) {
    grobs <- lapply(thetas, function(th) {
      xy <- rotate_df(base_xy, th)
      grid::polygonGrob(
        x = grid::unit(xy$V1, "native"),
        y = grid::unit(xy$V2, "native"),
        gp = grid::gpar(
          fill = grDevices::adjustcolor("black", alpha.f = a),
          col = NA
        )
      )
    })
    do.call(grid::gList, grobs)
  }

  grobs_minor <- make_polys(ang_minor, alpha_minor)
  grobs_major <- make_polys(ang_major, alpha_major)

  key <- as.numeric(key)
  vals <- c(min(key, na.rm = TRUE),
            stats::median(key, na.rm = TRUE),
            max(key, na.rm = TRUE))
  labs <- sprintf("%.2f", vals)

  vp <- grid::viewport(xscale = c(-6, 12), yscale = c(-11, 13))
  g_axis_x <- grid::segmentsGrob(
    grid::unit(-5, "native"),
    grid::unit(0, "native"),
    grid::unit(10, "native"),
    grid::unit(0, "native"),
    gp = grid::gpar(col = axis_col, alpha = axis_alpha)
  )
  g_axis_y <- grid::segmentsGrob(
    grid::unit(0, "native"),
    grid::unit(10.5, "native"),
    grid::unit(0, "native"),
    grid::unit(-10.5, "native"),
    gp = grid::gpar(col = axis_col, alpha = axis_alpha)
  )

  y_title <- if (inherits(title_lift, "unit"))
    grid::unit(12, "native") + title_lift
  else
    grid::unit(12 + title_lift, "native")
  g_title <- grid::textGrob(
    label,
    x = grid::unit(0, "native"),
    y = y_title,
    gp = grid::gpar(fontsize = 12)
  )

  pos_top <- c(0, 10.5) + lab_nudge_top
  pos_right <- c(10, 0.0) + lab_nudge_right
  pos_bottom <- c(0, -10.5) + lab_nudge_bottom

  g_lab_top <- grid::textGrob(
    labs[1],
    x = grid::unit(pos_top[1], "native"),
    y = grid::unit(pos_top[2], "native"),
    gp = grid::gpar(fontsize = font_size)
  )
  g_lab_right <- grid::textGrob(
    labs[2],
    x = grid::unit(pos_right[1], "native"),
    y = grid::unit(pos_right[2], "native"),
    gp = grid::gpar(fontsize = font_size)
  )
  g_lab_bottom <- grid::textGrob(
    labs[3],
    x = grid::unit(pos_bottom[1], "native"),
    y = grid::unit(pos_bottom[2], "native"),
    gp = grid::gpar(fontsize = font_size)
  )

  children <- do.call(grid::gList, c(
    list(
      grid::rectGrob(gp = grid::gpar(col = NA)),
      g_title,
      g_lab_top,
      g_lab_right,
      g_lab_bottom
    ),
    grobs_minor,
    grobs_major,
    list(g_axis_x, g_axis_y)
  ))

  main_tree <- grid::gTree(children = children,
                           vp = vp,
                           name = "glyph_key_tree")

  gt <- gtable::gtable(
    widths  = grid::unit.c(grid::unit(0, "cm"), grid::unit(2, "cm"), grid::unit(0, "cm")),
    heights = grid::unit.c(grid::unit(1, "cm"), grid::unit(2, "cm"), grid::unit(1, "cm"))
  )
  gtable::gtable_add_grob(
    gt,
    main_tree,
    t = 2,
    l = 2,
    clip = "off",
    name = "glyph_key"
  )
}
