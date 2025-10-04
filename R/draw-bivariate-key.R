draw_bivariate_key <- function(key,
                               size,
                               n_breaks = NULL,
                               prm_label = NULL,
                               scd_label = NULL,
                               prm_text = NULL,
                               scd_text = NULL) {
  layout <- expand.grid(row = 1:n_breaks[1], col = 1:n_breaks[2])

  tiles <- lapply(seq_len(prod(n_breaks)), function(i) {
    grid::rectGrob(
      x = grid::unit((layout$col[i] - 0.5) / n_breaks[1], "npc"),
      y = grid::unit((layout$row[i] - 0.5) / n_breaks[2], "npc"),
      width  = grid::unit(1 / n_breaks[1], "npc"),
      height = grid::unit(1 / n_breaks[2], "npc"),
      gp = grid::gpar(fill = key$fill[i], col = "black")
    )
  })

  grobs <- tiles

  offset_x <- 0.07
  offset_y <- -0.02

  if (!is.null(prm_label)) {
    for (i in 1:n_breaks[1]) {
      grobs[[length(grobs) + 1]] <- grid::textGrob(
        label = prm_label[i],
        x = grid::unit(seq(0, 1, length.out = n_breaks[1])[i], "npc") + grid::unit(offset_x, "npc"),
        y = grid::unit(-0.18, "npc") + grid::unit(offset_y, "npc"),
        just = "top",
        rot = -90,
        gp = grid::gpar(fontsize = 8)
      )
    }
  }

  if (!is.null(scd_label)) {
    for (i in 1:n_breaks[2]) {
      grobs[[length(grobs) + 1]] <- grid::textGrob(
        label = scd_label[i],
        x = grid::unit(-0.06, "npc"),
        y = grid::unit(seq(0, 1, length.out = n_breaks[2])[i], "npc"),
        just = "right",
        gp = grid::gpar(fontsize = 8)
      )
    }
  }

  if (!is.null(prm_text)) {
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = prm_text,
      x = grid::unit(-0.5, "npc"),
      y = grid::unit(0.5, "npc"),
      just = c("center", "center"),
      gp = grid::gpar(fontsize = 9, fontface = "bold"),
      rot = -90
    )
  }

  if (!is.null(scd_text)) {
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = scd_text,
      x = grid::unit(0.5, "npc"),
      y = grid::unit(-0.5, "npc"),
      just = c("center", "center"),
      gp = grid::gpar(fontsize = 9, fontface = "bold")
    )
  }

  grob <- grid::grobTree(
    children = do.call(grid::gList, grobs),
    vp = grid::viewport(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      width = unit(1, "npc"),
      height = unit(1, "npc"),
      angle = 45
    )
  )

  width <- grid::unit.c(unit(1, "cm"),
                        unit(size, "cm"),
                        unit(1, "cm"))

  height <- grid::unit.c(unit(1, "cm"),
                         unit(size, "cm"),
                         unit(1, "cm"))

  gt <- gtable::gtable(widths = width, heights = height)

  gt <- gtable::gtable_add_grob(
    gt,
    grob,
    t = 2,
    l = 2,
    clip = "off",
    name = "bivar_key"
  )

  return(gt)
}
