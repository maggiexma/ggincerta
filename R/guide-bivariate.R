#' @export
GuideBivariate <- ggproto(
  "GuideBivariate",
  GuideLegend,
  params = c(
    GuideLegend$params,
    list(
      key = NULL,
      n_breaks = NULL,
      label1 = NULL,
      label2 = NULL,
      title1 = NULL,
      title2 = NULL,
      aesthetics = NULL,
      size = NULL
    )
  ),
  extract_key = function(scale, aesthetic, key, ...) {
    key
  },
  draw = function(self, theme, params = self$params, ...) {

    draw_key_bivariate(
      key = params$key,
      size = params$size,
      n_breaks = params$n_breaks,
      label1 = params$label1,
      label2 = params$label2,
      title1 = params$title1,
      title2 = params$title2,
      aesthetics = params$aesthetics
    )
  }
)

guide_bivariate <- function(
  key,
  value,
  label,
  n_breaks = c(4, 4),
  label1 = NULL,
  label2 = NULL,
  title1 = NULL,
  title2 = NULL,
  size = NULL,
  ...,
  theme = NULL,
  title = waiver(),
  order = 0,
  position = NULL,
  aesthetics = NULL
) {

  key <- data.frame(key, .value = value, .label = label)

  new_guide(
    key = key,
    n_breaks = n_breaks,
    label1 = label1,
    label2 = label2,
    title1 = title1,
    title2 = title2,
    aesthetics = aesthetics,
    size = size,
    theme = theme,
    title = title,
    order = order,
    position = position,
    available_aes = c('fill', 'colour'),
    super = GuideBivariate
  )
}

draw_key_bivariate <- function(key,
                               size,
                               color = "transparent",
                               angle = 45,
                               n_breaks = NULL,
                               label1 = NULL,
                               label2 = NULL,
                               title1 = NULL,
                               title2 = NULL,
                               aesthetics = "fill",
                               label_fontsize = 8,
                               title_fontsize = 9,
                               label_margin = grid::unit(0.06, "npc"),
                               gap = grid::unit(0.02, "npc")) {
  layout <- expand.grid(row = 1:n_breaks[1], col = 1:n_breaks[2])
  tiles <- lapply(seq_len(prod(n_breaks)), function(i) {
    grid::rectGrob(
      x = grid::unit((layout$col[i] - 0.5) / n_breaks[1], "npc"),
      y = grid::unit((layout$row[i] - 0.5) / n_breaks[2], "npc"),
      width  = grid::unit(1 / n_breaks[1], "npc"),
      height = grid::unit(1 / n_breaks[2], "npc"),
      gp = grid::gpar(fill = key[[aesthetics]][i], col = color)
    )
  })
  grobs <- tiles

  measure_max_label_width_mm <- function(labels,
                                         fontsize = 8,
                                         family = NULL) {
    if (is.null(labels) || length(labels) == 0)
      return(0)
    grid::pushViewport(grid::viewport(gp = grid::gpar(
      fontsize = fontsize, fontfamily = family
    )))
    on.exit(grid::popViewport(), add = TRUE)
    max(sapply(as.character(labels), function(s) {
      grid::convertWidth(grid::unit(1, "strwidth", data = s), "mm", valueOnly = TRUE)
    }))
  }

  lab1_w_mm <- measure_max_label_width_mm(label1, fontsize = label_fontsize)
  lab2_w_mm <- measure_max_label_width_mm(label2, fontsize = label_fontsize)
  panel_mm <- size * 10
  lab1_w_npc_num <- if (panel_mm > 0)
    lab1_w_mm / panel_mm
  else
    0
  lab2_h_npc_num <- if (panel_mm > 0)
    lab2_w_mm / panel_mm
  else
    0

  if (!is.null(label1)) {
    ys <- seq(0, 1, length.out = n_breaks[1] + 1)
    for (i in seq_along(label1)) {
      grobs[[length(grobs) + 1]] <- grid::textGrob(
        label = label1[i],
        x = -label_margin,
        y = grid::unit(ys[i], "npc"),
        just = "right",
        gp = grid::gpar(fontsize = label_fontsize)
      )
    }
  }
  if (!is.null(label2)) {
    xs <- seq(0, 1, length.out = n_breaks[2] + 1)
    for (i in seq_along(label2)) {
      grobs[[length(grobs) + 1]] <- grid::textGrob(
        label = label2[i],
        x = grid::unit(xs[i], "npc") + grid::unit(0.07, "npc"),
        y = -label_margin + grid::unit(-0.1, "npc"),
        just = "top",
        rot = -90,
        gp = grid::gpar(fontsize = label_fontsize)
      )
    }
  }

  title1_x <- -(label_margin + grid::unit(lab1_w_npc_num, "npc") + gap)
  title2_y <- -(label_margin + grid::unit(lab2_h_npc_num, "npc") + gap)

  if (!is.null(title1)) {
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = title1,
      x = title1_x,
      y = grid::unit(0.5, "npc"),
      just = c("center", "center"),
      gp = grid::gpar(fontsize = title_fontsize, fontface = "bold"),
      rot = -90
    )
  }
  if (!is.null(title2)) {
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = title2,
      x = grid::unit(0.5, "npc"),
      y = title2_y,
      just = c("center", "center"),
      gp = grid::gpar(fontsize = title_fontsize, fontface = "bold"),
      rot = 0
    )
  }

  grob <- grid::grobTree(
    children = do.call(grid::gList, grobs),
    vp = grid::viewport(
      x = grid::unit(0.5, "npc"),
      y = grid::unit(0.5, "npc"),
      width  = grid::unit(1, "npc"),
      height = grid::unit(1, "npc"),
      angle = angle
    )
  )

  width  <- grid::unit.c(grid::unit(1, "cm"),
                         grid::unit(size, "cm"),
                         grid::unit(1, "cm"))
  height <- grid::unit.c(grid::unit(1, "cm"),
                         grid::unit(size, "cm"),
                         grid::unit(1, "cm"))
  gt <- gtable::gtable(widths = width, heights = height)
  gtable::gtable_add_grob(
    gt,
    grob,
    t = 2,
    l = 2,
    clip = "off",
    name = "bivar_key"
  )
}
