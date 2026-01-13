draw_key_vsup <- function(
    key,
    size = 5,
    layer_sizes,
    value_breaks,
    uncertainty_breaks,
    aesthetics = "fill",
    title_value = "Value",
    title_uncertainty = "Uncertainty",
    label_fontsize = 8,
    title_fontsize = 9
) {
  L <- length(layer_sizes)
  if (L == 0L) return(grid::nullGrob())

  fills <- key[[aesthetics]]

  end_idx <- cumsum(layer_sizes)
  start_idx <- c(1L, head(end_idx, -1L) + 1L)

  theta_start <- 4 * pi / 6
  theta_end   <- 2 * pi / 6

  r_center <- 0
  r_outer  <- 0.48
  r_levels <- seq(0.12, r_outer, length.out = L)

  arc_steps <- 40

  polar_to_npc <- function(r, theta) {
    x <- 0.5 + r * cos(theta)
    y <- 0.0 + r * sin(theta) + 0.55
    list(x = x, y = y)
  }

  grobs <- list()

  for (layer in seq_len(L)) {
    n_cells <- layer_sizes[layer]
    layer_cols <- fills[start_idx[layer]:end_idx[layer]]

    if (layer == 1L) {
      r_in  <- r_center
      r_out <- r_levels[1L]
    } else {
      r_in  <- r_levels[layer - 1L]
      r_out <- r_levels[layer]
    }

    theta_bound <- if (n_cells == 1L) {
      c(theta_start, theta_end)
    } else {
      seq(theta_start, theta_end, length.out = n_cells + 1L)
    }

    for (cell in seq_len(n_cells)) {
      if (n_cells == 1L) {
        t0 <- theta_start
        t1 <- theta_end
      } else {
        t0 <- theta_bound[cell]
        t1 <- theta_bound[cell + 1L]
      }

      th_outer <- seq(t0, t1, length.out = arc_steps)
      th_inner <- seq(t1, t0, length.out = arc_steps)

      po  <- polar_to_npc(r_out, th_outer)
      pin <- polar_to_npc(r_in,  th_inner)

      grobs[[length(grobs) + 1L]] <- grid::polygonGrob(
        x = c(po$x, pin$x),
        y = c(po$y, pin$y),
        default.units = "npc",
        gp = grid::gpar(fill = layer_cols[cell], col = NA)
      )
    }
  }

  r_tick <- r_levels[L] + 0.01
  th_arc <- seq(theta_start, theta_end, length.out = 100)
  p_arc <- polar_to_npc(r_tick, th_arc)
  grobs[[length(grobs) + 1L]] <- grid::linesGrob(
    x = p_arc$x, y = p_arc$y,
    default.units = "npc",
    gp = grid::gpar(col = "black", lwd = 1)
  )

  n_valticks <- length(value_breaks)
  if (n_valticks > 0L) {
    theta_ticks <- seq(theta_start, theta_end, length.out = n_valticks)
    tick_len <- 0.02

    for (i in seq_along(theta_ticks)) {

      th <- theta_ticks[i]
      p0 <- polar_to_npc(r_tick, th)
      p1 <- polar_to_npc(r_tick + tick_len, th)

      grobs[[length(grobs) + 1L]] <- grid::segmentsGrob(
        x0 = p0$x, y0 = p0$y,
        x1 = p1$x, y1 = p1$y,
        default.units = "npc",
        gp = grid::gpar(col = "black", lwd = 0.7)
      )

      keep_label <- (i == 1L) || (i == n_valticks) || (i %% 2 == 1L)
      if (!keep_label) next

      pt <- polar_to_npc(r_tick + 2 * tick_len, th)

      rot_deg <- th * 180 / pi - 90
      if (rot_deg < -90) rot_deg <- rot_deg + 180
      if (rot_deg >  90) rot_deg <- rot_deg - 180

      grobs[[length(grobs) + 1L]] <- grid::textGrob(
        label = value_breaks[i],
        x = pt$x, y = pt$y,
        default.units = "npc",
        rot = rot_deg,
        just = c("centre", "bottom"),
        gp = grid::gpar(fontsize = label_fontsize)
      )
    }
  }

  theta_axis <- theta_end

  nx <- -cos(theta_axis + pi / 2)
  ny <- -sin(theta_axis + pi / 2)

  shift_point <- function(pt, s) {
    list(x = pt$x + s * nx, y = pt$y + s * ny)
  }

  axis_shift  <- 0.02
  tick_len    <- 0.02
  label_shift <- 0.06

  p_axis0_base <- polar_to_npc(r_center, theta_axis)
  p_axis1_base <- polar_to_npc(r_levels[L], theta_axis)

  p_axis0 <- shift_point(p_axis0_base, axis_shift)
  p_axis1 <- shift_point(p_axis1_base, axis_shift)

  grobs[[length(grobs) + 1L]] <- grid::linesGrob(
    x = c(p_axis0$x, p_axis1$x),
    y = c(p_axis0$y, p_axis1$y),
    default.units = "npc",
    gp = grid::gpar(col = "black", lwd = 1)
  )

  r_ticks <- seq(r_center, r_levels[L], length.out = L + 1L)
  for (i in seq_along(r_ticks)) {
    p_base <- polar_to_npc(r_ticks[i], theta_axis)
    p_on_axis <- shift_point(p_base, axis_shift)
    p_tick_end <- shift_point(p_base, axis_shift + tick_len)

    grobs[[length(grobs) + 1L]] <- grid::segmentsGrob(
      x0 = p_on_axis$x, y0 = p_on_axis$y,
      x1 = p_tick_end$x, y1 = p_tick_end$y,
      default.units = "npc",
      gp = grid::gpar(col = "black", lwd = 0.7)
    )
  }

  labels <- uncertainty_breaks
  n_lab <- length(labels)
  for (j in seq_len(n_lab)) {
    rj <- r_ticks[n_lab - j + 1L]
    p_base <- polar_to_npc(rj, theta_axis)
    p_lab  <- shift_point(p_base, axis_shift + label_shift)

    grobs[[length(grobs) + 1L]] <- grid::textGrob(
      label = labels[j],
      x = p_lab$x, y = p_lab$y,
      default.units = "npc",
      gp = grid::gpar(fontsize = label_fontsize)
    )
  }

  angle_deg <- theta_axis * 180 / base::pi

  r_title_base <- (r_center + r_levels[L]) / 2
  p_title_base <- polar_to_npc(r_title_base, theta_axis)
  p_title <- shift_point(p_title_base, axis_shift + label_shift + 0.06)

  grobs[[length(grobs) + 1L]] <- grid::textGrob(
    label = title_uncertainty,
    x = p_title$x, y = p_title$y,
    default.units = "npc",
    rot = angle_deg,
    gp = grid::gpar(fontsize = title_fontsize, fontface = "bold")
  )

  th_mid <- (theta_start + theta_end) / 2
  p_val <- polar_to_npc(r_tick + 0.15, th_mid)
  grobs[[length(grobs) + 1L]] <- grid::textGrob(
    label = title_value,
    x = p_val$x, y = p_val$y,
    default.units = "npc",
    gp = grid::gpar(fontsize = title_fontsize, fontface = "bold")
  )

  fan_grob <- grid::grobTree(
    children = do.call(grid::gList, grobs),
    vp = grid::viewport(x = 0.5, y = 0.5, width = 1, height = 1)
  )

  width  <- grid::unit.c(grid::unit(0, "cm"), grid::unit(size, "cm"), grid::unit(0, "cm"))
  height <- grid::unit.c(grid::unit(2, "cm"), grid::unit(size, "cm"), grid::unit(0, "cm"))

  gt <- gtable::gtable(widths = width, heights = height)
  gtable::gtable_add_grob(gt, fan_grob, t = 2, l = 2, clip = "off", name = "vsup_key")
}
