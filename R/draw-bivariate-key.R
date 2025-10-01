draw_bivariate_key <- function(key,
                               size,
                               est_label = NULL,
                               err_label = NULL,
                               est_text = NULL,
                               err_text = NULL) {
  layout <- expand.grid(row = 1:3, col = 1:3)

  tiles <- lapply(seq_len(9), function(i) {
    grid::rectGrob(
      x = unit((layout$col[i] - 0.5) / 3, "npc"),
      y = unit((layout$row[i] - 0.5) / 3, "npc"),
      width  = unit(1 / 3, "npc"),
      height = unit(1 / 3, "npc"),
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
        x = unit(c(0, 1 / 3, 2 / 3, 1)[i], "npc") + unit(offset_x, "npc"),
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
        x = unit(-0.06, "npc"),
        y = unit(c(0, 1 / 3, 2 / 3, 1)[i], "npc"),
        just = "right",
        gp = grid::gpar(fontsize = 8)
      )
    }
  }

  if (!is.null(est_text)) {
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = est_text,
      x = unit(-0.5, "npc"),
      y = unit(0.5, "npc"),
      just = c("center", "center"),
      gp = grid::gpar(fontsize = 9, fontface = "bold"),
      rot = -90
    )
  }

  if (!is.null(err_text)) {
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = err_text,
      x = unit(0.5, "npc"),
      y = unit(-0.5, "npc"),
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
