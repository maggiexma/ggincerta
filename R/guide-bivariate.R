#' @export
GuideBivariate <- ggproto(
  "GuideBivariate",
  GuideLegend,
  params = c(
    GuideLegend$params,
    list(
      key = NULL,
      n_breaks = NULL,
      prm_label = NULL,
      scd_label = NULL,
      prm_text = NULL,
      scd_text = NULL,
      aesthetics = NULL,
      size = NULL
    )
  ),
  draw = function(self, theme, params = self$params, ...) {
    draw_key_bivariate(
      key = params$key,
      size = params$size,
      n_breaks = params$n_breaks,
      prm_label = params$prm_label,
      scd_label = params$scd_label,
      prm_text = params$prm_text,
      scd_text = params$scd_text,
      aesthetics = params$aesthetics
    )
  }
)

guide_bivariate <- function(key,
                            value,
                            label,
                            n_breaks = c(4, 4),
                            prm_label = NULL,
                            scd_label = NULL,
                            prm_text = NULL,
                            scd_text = NULL,
                            size = NULL,
                            ...,
                            theme = NULL,
                            title = waiver(),
                            order = 0,
                            position = NULL,
                            aesthetics = NULL) {
  key <- data.frame(key, .value = value, .label = label)

  new_guide(
    key = key,
    n_breaks = n_breaks,
    prm_label = prm_label,
    scd_label = scd_label,
    prm_text = prm_text,
    scd_text = scd_text,
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
                               n_breaks = NULL,
                               prm_label = NULL,
                               scd_label = NULL,
                               prm_text = NULL,
                               scd_text = NULL,
                               aesthetics = "fill") {
  layout <- expand.grid(row = 1:n_breaks[1], col = 1:n_breaks[2])
  tiles <- lapply(seq_len(prod(n_breaks)), function(i) {
    grid::rectGrob(
      x = grid::unit((layout$col[i] - 0.5) / n_breaks[1], "npc"),
      y = grid::unit((layout$row[i] - 0.5) / n_breaks[2], "npc"),
      width  = grid::unit(1 / n_breaks[1], "npc"),
      height = grid::unit(1 / n_breaks[2], "npc"),
      gp = grid::gpar(fill = key[[aesthetics]][i], col = "black")
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

