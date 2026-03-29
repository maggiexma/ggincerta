GuideBivariate <- ggproto(
  "GuideBivariate",
  GuideLegend,

  params = c(
    GuideLegend$params,
    list(
      bivar_n_breaks = NULL,
      var1_labels = NULL,
      var2_labels = NULL,
      var1_title = NULL,
      var2_title = NULL,
      aesthetics = NULL,
      rotated = FALSE,
      angle = 45
    )
  ),

  available_aes = c("fill", "colour", "color"),
  hashables = rlang::exprs(title, order, rotated, angle),

  get_bivariate_guide_info = function(self, scale) {
    if (inherits(scale, "ScaleBivariate")) scale$get_guide_info() else NULL
  },

  resolve_guide_info = function(self, scale, key) {
    attr(key, "bivariate_guide_info") %||% self$get_bivariate_guide_info(scale)
  },

  resolve_title = function(self, title, scale) {
    scale_name <- scale$name
    if (!is_waiver(scale_name) && !is.null(scale_name) && scale_name != "")
      return(scale_name)
    if (is_waiver(title))
      return(NULL)
    if (is.character(title) && length(title) == 1 && grepl("^duo\\s*\\(", title))
      return(NULL)
    title
  },

  make_text_grob = function(self, element, label,
                            x = 0.5, y = 0.5,
                            hjust = 0.5, vjust = 0.5,
                            angle = 0,
                            margin_x = FALSE, margin_y = FALSE) {
    if (is.null(element) || is.null(label) || length(label) == 0) return(zeroGrob())
    element_grob(element,
                 label = label, x = x, y = y,
                 hjust = hjust, vjust = vjust,
                 angle = angle, margin_x = margin_x, margin_y = margin_y)
  },

  measure_labels = function(self, labels, element, what = c("width", "height")) {
    what <- match.arg(what)
    if (is.null(labels) || length(labels) == 0) return(grid::unit(0, "mm"))
    measure_fn <- if (what == "width") grid::grobWidth else grid::grobHeight
    grobs <- lapply(as.character(labels), function(lbl) self$make_text_grob(element, lbl))
    do.call(grid::unit.pmax, lapply(grobs, measure_fn))
  },

  compute_bivariate_layout = function(self, params, elements) {
    text_el <- elements$text
    title_el <- elements$theme.title
    label_gap <- grid::unit(1, "mm")
    title_gap <- grid::unit(1, "mm")
    rotated <- isTRUE(params$rotated)

    var1_label_h <- self$measure_labels(params$var1_labels, text_el, "height")
    var1_label_w <- self$measure_labels(params$var1_labels, text_el, "width")
    var2_label_w <- self$measure_labels(params$var2_labels, text_el, "width")

    var1_title_h <- if (!is.null(params$var1_title)) {
      grid::grobHeight(self$make_text_grob(title_el, params$var1_title))
    } else grid::unit(0, "mm")

    var1_title_w_rot <- if (!is.null(params$var1_title)) {
      grid::grobWidth(self$make_text_grob(title_el, params$var1_title, angle = -90))
    } else grid::unit(0, "mm")

    var2_title_w <- if (!is.null(params$var2_title)) {
      grid::grobWidth(self$make_text_grob(title_el, params$var2_title, angle = 90))
    } else grid::unit(0, "mm")

    bottom_anno_h <- grid::unit(0, "mm")
    if (!is.null(params$var1_labels)) {
      bottom_anno_h <- bottom_anno_h +
        (if (rotated) var1_label_w else var1_label_h) + label_gap
    }
    if (!is.null(params$var1_title)) {
      bottom_anno_h <- bottom_anno_h +
        (if (rotated) var1_title_w_rot else var1_title_h) + title_gap
    }

    left_anno_w <- grid::unit(0, "mm")
    if (!is.null(params$var2_labels)) left_anno_w <- left_anno_w + var2_label_w + label_gap
    if (!is.null(params$var2_title))  left_anno_w <- left_anno_w + var2_title_w  + title_gap

    key_w <- grid::unit.pmax(elements$key_width,  grid::unit(2, "cm"))
    key_h <- grid::unit.pmax(elements$key_height, grid::unit(2, "cm"))

    list(
      text_el = text_el,
      title_el = title_el,
      label_gap = label_gap,
      title_gap = title_gap,
      var1_label_h = var1_label_h,
      var1_label_w = var1_label_w,
      var2_label_w = var2_label_w,
      var1_title_h = var1_title_h,
      var1_title_w_rot = var1_title_w_rot,
      var2_title_w = var2_title_w,
      left_anno_w = left_anno_w,
      bottom_anno_h = bottom_anno_h,
      key_w = key_w,
      key_h = key_h,
      decor_w = left_anno_w + key_w,
      decor_h = bottom_anno_h + key_h
    )
  },

  build_fill_matrix = function(self, fills, n_breaks) {
    t(matrix(fills, nrow = n_breaks[1], ncol = n_breaks[2]))
  },

  compute_panel_geometry = function(self, layout) {
    decor_w_mm <- grid::convertWidth(layout$decor_w,  "mm", valueOnly = TRUE)
    decor_h_mm <- grid::convertHeight(layout$decor_h, "mm", valueOnly = TRUE)

    to_npc_x <- function(u)
      grid::unit(grid::convertWidth(u,  "mm", valueOnly = TRUE) / decor_w_mm, "npc")
    to_npc_y <- function(u)
      grid::unit(grid::convertHeight(u, "mm", valueOnly = TRUE) / decor_h_mm, "npc")

    list(
      to_npc_x = to_npc_x,
      to_npc_y = to_npc_y,
      x = to_npc_x(layout$left_anno_w),
      y = to_npc_y(layout$bottom_anno_h),
      w = to_npc_x(layout$key_w),
      h = to_npc_y(layout$key_h)
    )
  },

  build_tile_grobs = function(self, fill_mat, panel, key_gp) {
    n_row <- nrow(fill_mat)
    n_col <- ncol(fill_mat)
    grobs <- vector("list", n_row * n_col)
    k <- 1L

    for (row in seq_len(n_row)) {
      for (col in seq_len(n_col)) {
        grobs[[k]] <- grid::rectGrob(
          x = panel$x + (col - 0.5) / n_col * panel$w,
          y = panel$y + (row - 0.5) / n_row * panel$h,
          width = panel$w / n_col,
          height = panel$h / n_row,
          gp = grid::gpar(
            fill = fill_mat[row, col],
            col = key_gp$col %||% NA,
            lwd = key_gp$lwd %||% 0.5,
            lty = key_gp$lty %||% 1
          )
        )
        k <- k + 1L
      }
    }
    grobs
  },

  build_var1_label_grobs = function(self, labels, panel, layout, params = NULL) {
    if (is.null(labels)) return(list())

    xs <- seq(0, 1, length.out = length(labels))
    rotated <- isTRUE(params$rotated)

    if (rotated) {
      y <- panel$to_npc_y(
        layout$bottom_anno_h - layout$label_gap - 0.5 * layout$var1_label_w
      )
      return(lapply(seq_along(labels), function(i) {
        self$make_text_grob(layout$text_el, labels[i],
                            x = panel$x + xs[i] * panel$w, y = y,
                            hjust = 0.5, vjust = 0.5, angle = -90)
      }))
    }

    y <- panel$to_npc_y(layout$bottom_anno_h - layout$label_gap)
    lapply(seq_along(labels), function(i) {
      self$make_text_grob(layout$text_el, labels[i],
                          x = panel$x + xs[i] * panel$w, y = y,
                          hjust = 0.5, vjust = 1)
    })
  },

  build_var2_label_grobs = function(self, labels, panel, layout) {
    if (is.null(labels)) return(list())

    ys <- seq(0, 1, length.out = length(labels))
    x <- panel$to_npc_x(layout$left_anno_w - layout$label_gap)

    lapply(seq_along(labels), function(i) {
      self$make_text_grob(layout$text_el, labels[i],
                          x = x, y = panel$y + ys[i] * panel$h,
                          hjust = 1, vjust = 0.5)
    })
  },

  build_var1_title_grob = function(self, title, labels, panel, layout, params = NULL) {
    if (is.null(title)) return(list())

    rotated <- isTRUE(params$rotated)
    y <- if (!is.null(labels)) {
      if (rotated) {
        layout$bottom_anno_h - layout$label_gap -
          layout$var1_label_w - layout$title_gap - 0.5 * layout$var1_title_h
      } else {
        layout$bottom_anno_h - layout$var1_label_h - layout$label_gap -
          layout$title_gap - 0.5 * layout$var1_title_h
      }
    } else {
      layout$bottom_anno_h - layout$title_gap - 0.5 * layout$var1_title_h
    }

    list(self$make_text_grob(layout$title_el, title,
                             x = panel$x + 0.5 * panel$w,
                             y = panel$to_npc_y(y),
                             hjust = 0.5, vjust = 0.5))
  },

  build_var2_title_grob = function(self, title, labels, panel, layout, params = NULL) {
    if (is.null(title)) return(list())

    x <- if (!is.null(labels)) {
      layout$left_anno_w - layout$var2_label_w - layout$label_gap -
        layout$title_gap - 0.5 * layout$var2_title_w
    } else {
      layout$left_anno_w - layout$title_gap - 0.5 * layout$var2_title_w
    }

    list(self$make_text_grob(layout$title_el, title,
                             x = panel$to_npc_x(x),
                             y = panel$y + 0.5 * panel$h,
                             hjust = 0.5, vjust = 0.5,
                             angle = if (isTRUE(params$rotated)) 270 else -90))
  },

  extract_key = function(self, scale, aesthetic, ...) {
    guide_info <- self$get_bivariate_guide_info(scale)
    if (is.null(guide_info)) return(NULL)

    n <- prod(guide_info$n_breaks)
    if (!length(n) || is.na(n) || n <= 0) return(NULL)

    key <- data.frame(
      .value = as.character(seq_len(n)),
      .label = as.character(seq_len(n)),
      stringsAsFactors = FALSE
    )
    key[[aesthetic]] <- guide_info$key_colours
    attr(key, "bivariate_guide_info") <- guide_info
    key
  },

  extract_params = function(self, scale, params, title = NULL, ...) {
    guide_info <- self$resolve_guide_info(scale, params$key)
    if (is.null(guide_info)) return(NULL)

    params$title <- self$resolve_title(title, scale)
    params$bivar_n_breaks <- guide_info$n_breaks
    params$var1_labels <- guide_info$x_labels %||% format(guide_info$x_breaks)
    params$var2_labels <- guide_info$y_labels %||% format(guide_info$y_breaks)
    params$var1_title <- guide_info$var1_title
    params$var2_title <- guide_info$var2_title
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

  build_labels = function(self, key, elements, params) zeroGrob(),

  build_decor = function(self, decor, grobs, elements, params) {
    if (is.null(params$key) || is.null(params$bivar_n_breaks)) return(zeroGrob())

    fill_mat <- self$build_fill_matrix(
      params$key[[params$aesthetics[1]]],
      params$bivar_n_breaks
    )
    layout_params <- params[c("var1_labels", "var2_labels", "var1_title", "var2_title")]
    layout <- self$compute_bivariate_layout(layout_params, elements)
    panel <- self$compute_panel_geometry(layout)
    key_gp <- elements$key$gp %||% grid::gpar()

    grob_list <- c(
      self$build_tile_grobs(fill_mat, panel, key_gp),
      self$build_var1_label_grobs(params$var1_labels, panel, layout, params),
      self$build_var2_label_grobs(params$var2_labels, panel, layout),
      self$build_var1_title_grob(params$var1_title, params$var1_labels, panel, layout, params),
      self$build_var2_title_grob(params$var2_title, params$var2_labels, panel, layout, params)
    )

    decor_grob <- grid::grobTree(children = do.call(grid::gList, grob_list))

    if (isTRUE(params$rotated)) {
      decor_grob <- grid::grobTree(
        decor_grob,
        vp = grid::viewport(
          x = 0.5, y = 0.5,
          width = grid::unit(1, "npc"),
          height = grid::unit(1, "npc"),
          angle = params$angle %||% 45
        )
      )
    }
    decor_grob
  },

  build_title_grob = function(self, params, elements) {
    title <- params$title
    if (is.null(title) || identical(title, "") || is_waiver(title))
      return(zeroGrob())
    self$make_text_grob(elements$theme.title, title)
  },

  measure_grobs = function(self, grobs, params, elements) {
    layout <- self$compute_bivariate_layout(params, elements)
    list(
      title_w = grid::grobWidth(grobs$title),
      title_h = grid::grobHeight(grobs$title),
      decor_w = layout$decor_w,
      decor_h = layout$decor_h,
      key_w = layout$key_w,
      key_h = layout$key_h,
      left_anno_w = layout$left_anno_w,
      bottom_anno_h = layout$bottom_anno_h
    )
  },

  arrange_layout = function(self, key, sizes, params, elements) {
    list(has_title = !inherits(sizes$title_h, "zeroUnit"),
         title_position = "top")
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    mg <- elements$margin %||% margin(0, 0, 0, 0, unit = "pt")
    mg_unit <- attr(mg, "unit")
    pad_t <- grid::unit(mg[[1]], mg_unit)
    pad_r <- grid::unit(mg[[2]], mg_unit)
    pad_b <- grid::unit(mg[[3]], mg_unit)
    pad_l <- grid::unit(mg[[4]], mg_unit)

    has_title <- !inherits(grobs$title, "zeroGrob")
    title_h <- if (has_title) grid::grobHeight(grobs$title) else grid::unit(0, "mm")
    title_gap <- if (has_title) grid::unit(2, "mm") else grid::unit(0, "mm")

    top_overhang <- grid::unit(0, "mm")
    if (isTRUE(params$rotated)) {
      angle_rad <- (params$angle %||% 45) * pi / 180
      rot_h <- abs(sin(angle_rad)) * sizes$key_w + abs(cos(angle_rad)) * sizes$key_h
      top_overhang <- grid::unit.pmax(grid::unit(0, "mm"), 0.5 * (rot_h - sizes$key_h))
    }

    gt <- gtable::gtable(
      widths = grid::unit.c(pad_l, sizes$decor_w, pad_r),
      heights = grid::unit.c(pad_t, title_h, title_gap, top_overhang, sizes$decor_h, pad_b)
    )
    if (has_title) {
      gt <- gtable::gtable_add_grob(gt, grobs$title, t = 2, l = 2, clip = "off", name = "title")
    }
    gtable::gtable_add_grob(gt, grobs$decor, t = 5, l = 2, clip = "off", name = "decor")
  }
)

guide_bivariate <- function(theme = NULL,
                            title = NULL,
                            order = 0,
                            position = NULL,
                            rotated = FALSE,
                            angle = 45) {
  new_guide(
    theme = theme,
    title = title,
    order = order,
    position = position,
    rotated = rotated,
    angle = angle,
    super = GuideBivariate
  )
}
