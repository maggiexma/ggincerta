GuideVSUP <- ggproto(
  "GuideVSUP",
  GuideLegend,

  params = c(
    GuideLegend$params,
    list(
      layer_sizes = NULL,
      value_breaks = NULL,
      uncertainty_breaks = NULL,
      title_value = NULL,
      title_uncertainty = NULL,
      aesthetics = NULL
    )
  ),

  available_aes = c("fill", "colour", "color"),
  hashables = rlang::exprs(title, order),

  get_vsup_info = function(self, scale) {
    if (!inherits(scale, "ScaleVSUP")) {
      return(NULL)
    }
    scale$get_guide_info()
  },

  resolve_title = function(self, title, scale) {
    title_chr <- if (inherits(title, "waiver")) {
      ""
    } else {
      paste(as.character(title), collapse = "")
    }

    scale_name_missing <- is.null(scale$name) ||
      is_waiver(scale$name)

    if (nzchar(title_chr) &&
        scale_name_missing &&
        grepl("^duo\\s*\\(", title_chr)) {
      return(NULL)
    }

    title
  },

  element_gpar = function(self, element, default = grid::gpar()) {
    if (is.null(element)) {
      return(default)
    }

    grid::gpar(
      fontsize = element$size %||% default$fontsize,
      col = element$colour %||% default$col,
      fontface = element$face %||% default$fontface,
      lineheight = element$lineheight %||% default$lineheight
    )
  },

  get_layout_info = function(self, elements) {
    text_gp <- self$element_gpar(
      elements$text,
      default = grid::gpar(
        fontsize = 8,
        col = "black",
        fontface = "plain",
        lineheight = 0.9
      )
    )

    title_gp <- self$element_gpar(
      elements$title,
      default = grid::gpar(
        fontsize = 9,
        col = "black",
        fontface = "bold",
        lineheight = 0.9
      )
    )

    key_width <- elements$key_width
    key_height <- elements$key_height

    min_key_width <- grid::unit(4, "cm")
    min_key_height <- grid::unit(4, "cm")

    if (grid::convertWidth(key_width, "cm", valueOnly = TRUE) <
        grid::convertWidth(min_key_width, "cm", valueOnly = TRUE)) {
      key_width <- min_key_width
    }

    if (grid::convertHeight(key_height, "cm", valueOnly = TRUE) <
        grid::convertHeight(min_key_height, "cm", valueOnly = TRUE)) {
      key_height <- min_key_height
    }

    list(
      text_gp = text_gp,
      title_gp = title_gp,
      key_width = key_width,
      key_height = key_height,
      decor_width = key_width,
      decor_height = key_height
    )
  },

  extract_key = function(self, scale, aesthetic, ...) {
    guide_info <- self$get_vsup_info(scale)
    if (is.null(guide_info)) {
      return(NULL)
    }

    n_keys <- length(guide_info$key_colours)

    key <- data.frame(
      .value = as.character(seq_len(n_keys)),
      .label = as.character(seq_len(n_keys)),
      stringsAsFactors = FALSE
    )

    key[[aesthetic]] <- guide_info$key_colours
    key
  },

  extract_params = function(self, scale, params, title = NULL, ...) {
    params <- ggproto_parent(GuideLegend, self)$extract_params(scale = scale,
                                                               params = params,
                                                               title = title,
                                                               ...)

    guide_info <- self$get_vsup_info(scale)
    if (is.null(guide_info)) {
      return(params)
    }

    params$title <- self$resolve_title(title = params$title %||% scale$name %||% waiver(),
                                       scale = scale)

    params$layer_sizes <- guide_info$layer_sizes
    params$value_breaks <- format(guide_info$value_breaks, digits = 2)
    params$uncertainty_breaks <- format(guide_info$uncertainty_breaks, digits = 2)
    params$title_value <- guide_info$title_value
    params$title_uncertainty <- guide_info$title_uncertainty
    params$aesthetics <- guide_info$aesthetics

    params
  },

  setup_params = function(self, params) {
    params <- ggproto_parent(GuideLegend, self)$setup_params(params)

    if (is.null(params$aesthetics) && !is.null(params$key)) {
      params$aesthetics <- intersect(names(params$key), c("fill", "colour", "color"))
    }

    params
  },

  build_labels = function(self, key, elements, params) {
    zeroGrob()
  },

  build_decor = function(self, decor, grobs, elements, params) {
    if (is.null(params$key) || is.null(params$layer_sizes)) {
      return(zeroGrob())
    }

    aesthetic <- params$aesthetics[1]

    key <- data.frame(
      value = params$key$.value,
      label = params$key$.label,
      stringsAsFactors = FALSE
    )
    key[[aesthetic]] <- params$key[[aesthetic]]

    self$draw_vsup_decor(
      key = key,
      layer_sizes = params$layer_sizes,
      value_breaks = params$value_breaks,
      uncertainty_breaks = params$uncertainty_breaks,
      aesthetics = aesthetic,
      title_value = params$title_value,
      title_uncertainty = params$title_uncertainty,
      elements = elements
    )
  },

  build_title_grob = function(self, params, elements) {
    title <- params$title

    if (is.null(title) ||
        identical(title, "") || inherits(title, "waiver")) {
      return(zeroGrob())
    }

    grid::textGrob(
      label = title,
      gp = self$element_gpar(elements$title, default = grid::gpar())
    )
  },

  measure_grobs = function(self, grobs, params, elements) {
    layout_info <- self$get_layout_info(elements)

    list(
      title_width = grid::grobWidth(grobs$title),
      title_height = grid::grobHeight(grobs$title),
      decor_width = layout_info$decor_width,
      decor_height = layout_info$decor_height
    )
  },

  arrange_layout = function(self, key, sizes, params, elements) {
    list(
      has_title = !inherits(sizes$title_height, "zeroUnit"),
      title_position = "top"
    )
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    theme_margin <- elements$margin %||% margin(0, 0, 0, 0)
    unit_name <- attr(theme_margin, "unit")

    margin_top <- grid::unit(theme_margin[[1]], unit_name)
    margin_right <- grid::unit(theme_margin[[2]], unit_name)
    margin_bottom <- grid::unit(theme_margin[[3]], unit_name)
    margin_left <- grid::unit(theme_margin[[4]], unit_name)

    has_title <- !inherits(grobs$title, "zeroGrob")
    title_gap <- grid::unit(2, "mm")
    title_height <- if (has_title) grid::grobHeight(grobs$title) else grid::unit(0, "mm")

    widths <- grid::unit.c(
      margin_left,
      sizes$decor_width,
      margin_right
    )

    if (has_title) {
      heights <- grid::unit.c(
        margin_top,
        title_height,
        title_gap,
        sizes$decor_height,
        margin_bottom
      )

      gt <- gtable::gtable(widths = widths, heights = heights)

      gt <- gtable::gtable_add_grob(
        gt, grobs$title,
        t = 2, l = 2,
        clip = "off", name = "title"
      )

      gt <- gtable::gtable_add_grob(
        gt, grobs$decor,
        t = 4, l = 2,
        clip = "off", name = "decor"
      )
    } else {
      heights <- grid::unit.c(
        margin_top,
        sizes$decor_height,
        margin_bottom
      )

      gt <- gtable::gtable(widths = widths, heights = heights)

      gt <- gtable::gtable_add_grob(
        gt, grobs$decor,
        t = 2, l = 2,
        clip = "off", name = "decor"
      )
    }

    gt
  },

  draw_vsup_decor = function(self,
                             key,
                             layer_sizes,
                             value_breaks,
                             uncertainty_breaks,
                             aesthetics = "fill",
                             title_value = "Value",
                             title_uncertainty = "Uncertainty",
                             elements) {
    n_layers <- length(layer_sizes)
    if (n_layers == 0L) {
      return(grid::nullGrob())
    }

    layout_info <- self$get_layout_info(elements)
    fills <- key[[aesthetics]]

    end_idx <- cumsum(layer_sizes)
    start_idx <- c(1L, head(end_idx, -1L) + 1L)

    theta_start <- 4 * pi / 6
    theta_end <- 2 * pi / 6

    r_center <- 0
    r_outer <- 0.50
    r_levels <- seq(0.14, r_outer, length.out = n_layers)
    arc_steps <- 60

    polar_to_npc <- function(r, theta) {
      list(x = 0.4 + r * cos(theta), y = 0.25 + r * sin(theta))
    }

    grobs <- list()
    add_grob <- function(grob)
      grobs[[length(grobs) + 1L]] <<- grob

    for (layer in seq_len(n_layers)) {
      n_cells <- layer_sizes[layer]
      layer_fills <- fills[start_idx[layer]:end_idx[layer]]

      if (layer == 1L) {
        r_inner <- r_center
        r_outer_layer <- r_levels[1L]
      } else {
        r_inner <- r_levels[layer - 1L]
        r_outer_layer <- r_levels[layer]
      }

      theta_bounds <- if (n_cells == 1L) {
        c(theta_start, theta_end)
      } else {
        seq(theta_start, theta_end, length.out = n_cells + 1L)
      }

      for (cell in seq_len(n_cells)) {
        if (n_cells == 1L) {
          theta0 <- theta_start
          theta1 <- theta_end
        } else {
          theta0 <- theta_bounds[cell]
          theta1 <- theta_bounds[cell + 1L]
        }

        theta_outer <- seq(theta0, theta1, length.out = arc_steps)
        theta_inner <- seq(theta1, theta0, length.out = arc_steps)

        p_outer <- polar_to_npc(r_outer_layer, theta_outer)
        p_inner <- polar_to_npc(r_inner, theta_inner)

        add_grob(grid::polygonGrob(
          x = c(p_outer$x, p_inner$x),
          y = c(p_outer$y, p_inner$y),
          default.units = "npc",
          gp = grid::gpar(fill = layer_fills[cell], col = NA)
        ))
      }
    }

    text_gp <- layout_info$text_gp
    title_gp <- layout_info$title_gp

    r_tick <- r_levels[n_layers] + 0.015
    theta_arc <- seq(theta_start, theta_end, length.out = 120)
    p_arc <- polar_to_npc(r_tick, theta_arc)

    add_grob(grid::linesGrob(
      x = p_arc$x,
      y = p_arc$y,
      default.units = "npc",
      gp = grid::gpar(col = "black", lwd = 1)
    ))

    n_value_ticks <- length(value_breaks)
    if (n_value_ticks > 0L) {
      theta_ticks <- seq(theta_start, theta_end, length.out = n_value_ticks)
      tick_length <- 0.02

      for (i in seq_along(theta_ticks)) {
        theta <- theta_ticks[i]
        p0 <- polar_to_npc(r_tick, theta)
        p1 <- polar_to_npc(r_tick + tick_length, theta)

        add_grob(
          grid::segmentsGrob(
            x0 = p0$x,
            y0 = p0$y,
            x1 = p1$x,
            y1 = p1$y,
            default.units = "npc",
            gp = grid::gpar(col = "black", lwd = 0.7)
          )
        )

        keep_label <- (i == 1L) ||
          (i == n_value_ticks) || (i %% 2 == 1L)
        if (!keep_label) {
          next
        }

        p_label <- polar_to_npc(r_tick + 0.045, theta)
        rot_deg <- theta * 180 / pi - 90
        if (rot_deg < -90)
          rot_deg <- rot_deg + 180
        if (rot_deg > 90)
          rot_deg <- rot_deg - 180

        add_grob(
          grid::textGrob(
            label = value_breaks[i],
            x = p_label$x,
            y = p_label$y,
            default.units = "npc",
            rot = rot_deg,
            just = c("centre", "bottom"),
            gp = text_gp
          )
        )
      }
    }

    theta_axis <- theta_end
    nx <- -cos(theta_axis + pi / 2)
    ny <- -sin(theta_axis + pi / 2)

    shift_point <- function(point, shift) {
      list(x = point$x + shift * nx, y = point$y + shift * ny)
    }

    axis_shift <- 0.015
    tick_length <- 0.015
    label_shift <- 0.05

    p_axis_start_base <- polar_to_npc(r_center, theta_axis)
    p_axis_end_base <- polar_to_npc(r_levels[n_layers], theta_axis)

    p_axis_start <- shift_point(p_axis_start_base, axis_shift)
    p_axis_end <- shift_point(p_axis_end_base, axis_shift)

    add_grob(grid::linesGrob(
      x = c(p_axis_start$x, p_axis_end$x),
      y = c(p_axis_start$y, p_axis_end$y),
      default.units = "npc",
      gp = grid::gpar(col = "black", lwd = 1)
    ))

    r_ticks <- seq(r_center, r_levels[n_layers], length.out = n_layers + 1L)
    for (i in seq_along(r_ticks)) {
      p_base <- polar_to_npc(r_ticks[i], theta_axis)
      p_tick_start <- shift_point(p_base, axis_shift)
      p_tick_end <- shift_point(p_base, axis_shift + tick_length)

      add_grob(
        grid::segmentsGrob(
          x0 = p_tick_start$x,
          y0 = p_tick_start$y,
          x1 = p_tick_end$x,
          y1 = p_tick_end$y,
          default.units = "npc",
          gp = grid::gpar(col = "black", lwd = 0.7)
        )
      )
    }

    n_uncertainty_labels <- length(uncertainty_breaks)
    for (j in seq_len(n_uncertainty_labels)) {
      rj <- r_ticks[n_uncertainty_labels - j + 1L]
      p_base <- polar_to_npc(rj, theta_axis)
      p_label <- shift_point(p_base, axis_shift + label_shift)

      add_grob(
        grid::textGrob(
          label = uncertainty_breaks[j],
          x = p_label$x,
          y = p_label$y,
          default.units = "npc",
          just = c("left", "center"),
          gp = text_gp
        )
      )
    }

    if (!is.null(title_uncertainty) &&
        !identical(title_uncertainty, "")) {
      y_title <- (p_axis_start$y + p_axis_end$y) / 2 - 0.05

      p_title_base <- polar_to_npc(r_levels[n_layers], theta_axis)
      p_title <- shift_point(p_title_base, axis_shift + label_shift + 0.14)

      add_grob(
        grid::textGrob(
          label = title_uncertainty,
          x = p_title$x,
          y = y_title,
          default.units = "npc",
          rot = theta_axis * 180 / pi,
          just = c("center", "center"),
          gp = title_gp
        )
      )
    }

    if (!is.null(title_value) && !identical(title_value, "")) {
      theta_mid <- (theta_start + theta_end) / 2
      p_value_title <- polar_to_npc(r_tick + 0.16, theta_mid)

      add_grob(
        grid::textGrob(
          label = title_value,
          x = p_value_title$x,
          y = p_value_title$y,
          default.units = "npc",
          just = c("center", "center"),
          gp = title_gp
        )
      )
    }

    grid::grobTree(
      children = do.call(grid::gList, grobs),
      vp = grid::viewport(
        x = 0.5,
        y = 0.5,
        width = grid::unit(1, "npc"),
        height = grid::unit(1, "npc")
      )
    )
  }
)

guide_vsup <- function(theme = NULL,
                       title = waiver(),
                       order = 0,
                       position = NULL) {
  new_guide(
    theme = theme,
    title = title,
    order = order,
    position = position,
    super = GuideVSUP
  )
}
