GuideBivariate <- ggproto(
  "GuideBivariate",
  Guide,

  params = c(
    Guide$params,
    list(
      n_breaks = NULL,
      var1_labels = NULL,
      var2_labels = NULL,
      var1_title = NULL,
      var2_title = NULL,
      aesthetics = NULL,
      key_size = NULL
    )
  ),

  available_aes = c("fill", "colour", "color"),

  hashables = rlang::exprs(title, order),

  extract_key = function(self, scale, aesthetic, ...) {
    if (!inherits(scale, "ScaleBivariate")) {
      return(NULL)
    }

    guide_info <- scale$get_guide_info()
    if (is.null(guide_info)) {
      return(NULL)
    }

    data.frame(
      .value = as.character(seq_len(prod(
        guide_info$n_breaks
      ))),
      .label = as.character(seq_len(prod(
        guide_info$n_breaks
      ))),
      stringsAsFactors = FALSE
    )
  },

  extract_decor = function(self, scale, aesthetic, ...) {
    if (!inherits(scale, "ScaleBivariate")) {
      return(NULL)
    }

    guide_info <- scale$get_guide_info()
    if (is.null(guide_info)) {
      return(NULL)
    }

    data.frame(colour = guide_info$key_colours,
               stringsAsFactors = FALSE)
  },

  extract_params = function(self, scale, params, ...) {
    if (!inherits(scale, "ScaleBivariate")) {
      return(params)
    }

    guide_info <- scale$get_guide_info()
    if (is.null(guide_info)) {
      return(params)
    }

    params$n_breaks <- guide_info$n_breaks
    params$var1_labels <- format(guide_info$x_breaks, digits = 2)
    params$var2_labels <- format(guide_info$y_breaks, digits = 2)
    params$var1_title <- guide_info$var1_title
    params$var2_title <- guide_info$var2_title
    params$aesthetics <- guide_info$aesthetics
    params$key_size <- guide_info$key_size
    params
  },

  draw = function(self,
                  theme,
                  position = NULL,
                  direction = NULL,
                  params = self$params) {
    if (is.null(params$decor) || is.null(params$n_breaks)) {
      return(grid::zeroGrob())
    }

    aes_name <- params$aesthetics[1]

    key <- data.frame(
      value = params$key$.value,
      label = params$key$.label,
      stringsAsFactors = FALSE
    )
    key[[aes_name]] <- params$decor$colour

    draw_key_bivariate(
      key = key,
      key_size = params$key_size,
      n_breaks = params$n_breaks,
      var1_labels = params$var1_labels,
      var2_labels = params$var2_labels,
      var1_title = params$var1_title,
      var2_title = params$var2_title,
      aesthetics = aes_name
    )
  }
)

guide_bivariate <- function(theme = NULL,
                            title = waiver(),
                            order = 0,
                            position = NULL) {
  new_guide(
    theme = theme,
    title = title,
    order = order,
    position = position,
    super = GuideBivariate
  )
}

draw_key_bivariate <- function(key,
                               key_size,
                               colour = "transparent",
                               angle = 45,
                               n_breaks = NULL,
                               var1_labels = NULL,
                               var2_labels = NULL,
                               var1_title = NULL,
                               var2_title = NULL,
                               aesthetics = "fill",
                               label_fontsize = 8,
                               title_fontsize = 9,
                               label_margin = grid::unit(0.06, "npc"),
                               title_gap = grid::unit(0.3, "npc")) {
  n_row <- n_breaks[1]
  n_col <- n_breaks[2]

  fills <- key[[aesthetics]]

  fill_mat <- matrix(fills,
                     nrow = n_row,
                     ncol = n_col,
                     byrow = FALSE)

  grobs <- list()

  for (row in seq_len(n_row)) {
    for (col in seq_len(n_col)) {
      grobs[[length(grobs) + 1]] <- grid::rectGrob(
        x = grid::unit((col - 0.5) / n_col, "npc"),
        y = grid::unit((row - 0.5) / n_row, "npc"),
        width = grid::unit(1 / n_col, "npc"),
        height = grid::unit(1 / n_row, "npc"),
        gp = grid::gpar(fill = fill_mat[row, col], col  = colour)
      )
    }
  }

  measure_max_label_width_mm <- function(labels,
                                         fontsize = 8,
                                         family = NULL) {
    if (is.null(labels) || length(labels) == 0)
      return(0)
    grid::pushViewport(grid::viewport(gp = grid::gpar(
      fontsize = fontsize, fontfamily = family
    )))
    on.exit(grid::popViewport(), add = TRUE)
    max(vapply(as.character(labels), function(s) {
      grid::convertWidth(grid::unit(1, "strwidth", data = s), "mm", valueOnly = TRUE)
    }, numeric(1)))
  }

  var1_label_width_mm <- measure_max_label_width_mm(var1_labels, fontsize = label_fontsize)
  var2_label_width_mm <- measure_max_label_width_mm(var2_labels, fontsize = label_fontsize)

  panel_mm <- key_size * 10
  var1_label_width_npc <- if (panel_mm > 0)
    var1_label_width_mm / panel_mm
  else
    0
  var2_label_height_npc <- if (panel_mm > 0)
    var2_label_width_mm / panel_mm
  else
    0

  if (!is.null(var1_labels)) {
    ys <- seq(0, 1, length.out = length(var1_labels))
    for (i in seq_along(var1_labels)) {
      grobs[[length(grobs) + 1]] <- grid::textGrob(
        label = var1_labels[i],
        x = -label_margin,
        y = grid::unit(ys[i], "npc"),
        just = "right",
        gp = grid::gpar(fontsize = label_fontsize)
      )
    }
  }

  if (!is.null(var2_labels)) {
    xs <- seq(0, 1, length.out = length(var2_labels))
    for (i in seq_along(var2_labels)) {
      grobs[[length(grobs) + 1]] <- grid::textGrob(
        label = var2_labels[i],
        x = grid::unit(xs[i], "npc") + grid::unit(0.07, "npc"),
        y = -label_margin + grid::unit(-0.15, "npc"),
        just = "top",
        rot = -90,
        gp = grid::gpar(fontsize = label_fontsize)
      )
    }
  }

  var1_title_x <- -(label_margin + grid::unit(var1_label_width_npc, "npc") + title_gap)
  var2_title_y <- -(label_margin + grid::unit(var2_label_height_npc, "npc") + title_gap)

  if (!is.null(var1_title)) {
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = var1_title,
      x = var1_title_x,
      y = grid::unit(0.5, "npc"),
      just = c("center", "center"),
      gp = grid::gpar(fontsize = title_fontsize, fontface = "bold"),
      rot = -90
    )
  }

  if (!is.null(var2_title)) {
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = var2_title,
      x = grid::unit(0.5, "npc"),
      y = var2_title_y,
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
      width = grid::unit(1, "snpc"),
      height = grid::unit(1, "snpc"),
      angle = angle
    )
  )

  widths <- grid::unit.c(grid::unit(1, "cm"),
                          grid::unit(key_size, "cm"),
                          grid::unit(1, "cm"))
  heights <- grid::unit.c(grid::unit(1, "cm"),
                          grid::unit(key_size, "cm"),
                          grid::unit(1, "cm"))

  gt <- gtable::gtable(widths = widths, heights = heights)
  gt <- gtable::gtable_add_grob(
    gt,
    grob,
    t = 2,
    l = 2,
    clip = "off",
    name = "bivariate_key"
  )
  gt
}
