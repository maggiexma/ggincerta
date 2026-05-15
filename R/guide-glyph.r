GuideGlyph <- ggproto(
  "GuideGlyph",
  Guide,

  params = list(
    name = "glyph",
    title = waiver(),
    theme = NULL,
    direction = "vertical",
    order = 99,
    position = NULL,
    hash = "glyph-angle",
    available_aes = "angle"
  ),

  train = function(self, params = self$params, scale, aesthetic = NULL, ...) {
    lims <- scale$get_limits()
    lims <- lims[is.finite(lims)]

    if (!length(lims)) {
      return(NULL)
    }

    maxv <- max(abs(lims), na.rm = TRUE)

    key <- c(0, maxv / 2, maxv)

    params$title <- if (is_waiver(params$title)) scale$name else params$title
    if (is_waiver(params$title)) params$title <- NULL

    params$key <- data.frame(
      angle = key,
      .value = key,
      .label = format(round(key, 2), trim = TRUE)
    )

    params$breaks <- key
    params$labels <- format(round(key, 2), trim = TRUE)
    params$angles <- c(0, -pi / 2, -pi)
    params$hash <- paste0("glyph-angle-", paste(round(key, 3), collapse = "-"))

    params
  },

  draw = function(self, theme, params = self$params, ...) {
    title_el <- calc_element("legend.title", theme)
    text_el <- calc_element("legend.text", theme)

    draw_glyph_key(
      title = params$title,
      labels = params$labels,
      angles = params$angles,
      title_gp = grid::gpar(
        fontsize = title_el$size,
        fontface = title_el$face,
        col = title_el$colour
      ),
      text_gp = grid::gpar(
        fontsize = text_el$size,
        fontface = text_el$face,
        col = text_el$colour
      )
    )
  }
)

guide_glyph <- function(theme = NULL,
                        title = waiver(),
                        order = 99,
                        position = NULL,
                        direction = "vertical") {
  new_guide(
    name = "glyph",
    title = title,
    theme = theme,
    direction = direction,
    order = order,
    position = position,
    hash = "glyph-angle",
    available_aes = "angle",
    super = GuideGlyph
  )
}

draw_glyph_key <- function(title = NULL,
                           labels = c("0", "0.5", "1"),
                           angles = c(0, -pi / 2, -pi),
                           title_gp = grid::gpar(fontsize = 11),
                           text_gp = grid::gpar(fontsize = 9),
                           minor_n = 6,
                           alpha_major = 0.70,
                           alpha_minor = 0.20,
                           axis_col = "black",
                           axis_alpha = 0.45) {
  x0 <- seq(-3, 3, by = 0.05)
  y0 <- sqrt(pmax(0, 9 - x0^2)) + 5

  base_xy <- rbind(
    data.frame(x = x0, y = y0),
    data.frame(x = c(-3, 0, 3), y = c(5, 0, 5))
  )

  rot <- function(th) {
    matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2)
  }

  rotate_df <- function(xy, th) {
    as.data.frame(t(rot(th) %*% t(as.matrix(xy))))
  }

  make_polys <- function(thetas, alpha) {
    grobs <- lapply(thetas, function(th) {
      xy <- rotate_df(base_xy, th)

      grid::polygonGrob(
        x = grid::unit(xy$V1, "native"),
        y = grid::unit(xy$V2, "native"),
        gp = grid::gpar(
          fill = grDevices::adjustcolor("black", alpha.f = alpha),
          col = NA
        )
      )
    })

    do.call(grid::gList, grobs)
  }

  grobs_minor <- make_polys(seq(0, -pi, length.out = minor_n), alpha_minor)
  grobs_major <- make_polys(angles, alpha_major)

  g_title <- if (!is.null(title) && !identical(title, "")) {
    grid::textGrob(
      title,
      x = grid::unit(0, "native"),
      y = grid::unit(17.5, "native"),
      gp = title_gp
    )
  } else {
    zeroGrob()
  }

  g_labels <- list(
    grid::textGrob(labels[1], x = grid::unit(0, "native"), y = grid::unit(12.5, "native"), gp = text_gp),
    grid::textGrob(labels[2], x = grid::unit(14.5, "native"), y = grid::unit(0, "native"), gp = text_gp),
    grid::textGrob(labels[3], x = grid::unit(0, "native"), y = grid::unit(-12.5, "native"), gp = text_gp)
  )

  g_axis_x <- grid::segmentsGrob(
    grid::unit(-6, "native"),
    grid::unit(0, "native"),
    grid::unit(11.5, "native"),
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

  main_tree <- grid::gTree(
    children = do.call(grid::gList, c(
      list(grid::rectGrob(gp = grid::gpar(col = NA)), g_title),
      g_labels,
      grobs_minor,
      grobs_major,
      list(g_axis_x, g_axis_y)
    )),
    vp = grid::viewport(
      xscale = c(-8, 20),
      yscale = c(-16, 20)
    )
  )

  gt <- gtable::gtable(
    widths = grid::unit.c(
      grid::unit(0, "cm"),
      grid::unit(2.8, "cm"),
      grid::unit(0, "cm")
    ),
    heights = grid::unit.c(
      grid::unit(0.3, "cm"),
      grid::unit(3.2, "cm"),
      grid::unit(0.3, "cm")
    )
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

