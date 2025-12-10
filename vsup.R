vsup_mix_colors <- function(base_col, target_col = "#F5F5F5", alpha = 1) {
  base_rgb <- grDevices::col2rgb(base_col) / 255
  target_rgb <- grDevices::col2rgb(target_col) / 255
  out_rgb <- alpha * base_rgb + (1 - alpha) * target_rgb
  grDevices::rgb(out_rgb[1, ], out_rgb[2, ], out_rgb[3, ])
}

vsup_square_palette <- function(
    n = 3,
    mode = c("usl", "us", "ul"),
    value_pal_fun = viridisLite::viridis,
    grey_target = "#F5F5F5"
) {
  mode <- match.arg(mode)
  value_cols <- value_pal_fun(n)
  pal_mat <- matrix(NA_character_, nrow = n, ncol = n)
  for (j in seq_len(n)) {
    u <- (j - 1) / (n - 1)
    keep <- switch(
      mode,
      "usl" = 1 - u^1.2,
      "us" = 1 - u,
      "ul" = 1 - u^0.7
    )
    for (i in seq_len(n)) {
      base_col <- value_cols[i]
      pal_mat[j, i] <- vsup_mix_colors(base_col, grey_target, alpha = keep)
    }
  }
  pal_mat
}


scale_fill_vsup <- function(
    ...,
    n = 3,
    mode = c("usl", "us", "ul"),
    value_pal_fun = viridisLite::viridis,
    grey_target = "#F5F5F5",
    guide = "vsup"
) {
  mode <- match.arg(mode)
  pal_mat <- vsup_square_palette(
    n = n,
    mode = mode,
    value_pal_fun = value_pal_fun,
    grey_target = grey_target
  )
  pal_vec <- as.vector(t(pal_mat))
  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "vsup",
    palette = function(x) {
      idx <- as.integer(as.character(x))
      pal_vec[idx]
    },
    guide = guide,
    ...
  )
}

vsup_legend_square_grob <- function(
    pal_mat,
    value_breaks = NULL,
    unc_breaks = NULL,
    value_label = "Value",
    uncertainty_label = "Uncertainty"
) {
  n <- nrow(pal_mat)
  stopifnot(ncol(pal_mat) == n)
  if (is.null(value_breaks)) {
    value_breaks <- signif(seq(0, 1, length.out = n), 2)
  }
  if (is.null(unc_breaks)) {
    unc_breaks <- signif(seq(0, 1, length.out = n), 2)
  }
  vp <- grid::viewport(
    x = 0.5, y = 0.5,
    width = 1, height = 1,
    just = c("center", "center"),
    name = "vsup-legend"
  )
  grobs <- list()
  for (j in seq_len(n)) {
    for (i in seq_len(n)) {
      x <- (i - 0.5) / n
      y <- (j - 0.5) / n
      grobs[[length(grobs) + 1]] <- grid::rectGrob(
        x = x, y = y,
        width = 1 / n, height = 1 / n,
        gp = grid::gpar(
          fill = pal_mat[j, i],
          col  = "grey70",
          lwd  = 0.3
        )
      )
    }
  }
  for (i in seq_len(n)) {
    x <- (i - 0.5) / n
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = value_breaks[i],
      x = x, y = -0.07,
      gp = grid::gpar(cex = 0.6)
    )
  }
  for (j in seq_len(n)) {
    y <- (j - 0.5) / n
    grobs[[length(grobs) + 1]] <- grid::textGrob(
      label = unc_breaks[j],
      x = -0.07, y = y,
      gp = grid::gpar(cex = 0.6)
    )
  }
  grobs[[length(grobs) + 1]] <- grid::textGrob(
    label = value_label,
    x = 0.5, y = -0.18,
    gp = grid::gpar(cex = 0.75)
  )
  grobs[[length(grobs) + 2]] <- grid::textGrob(
    label = uncertainty_label,
    x = -0.18, y = 0.5,
    rot = 90,
    gp = grid::gpar(cex = 0.75)
  )
  grid::grobTree(children = do.call(grid::gList, grobs), vp = vp)
}


library(ggplot2)
library(grid)

n <- 3
pal_mat <- vsup_square_palette(n = n, mode = "usl")

df <- expand.grid(
  v = factor(1:n),
  u = factor(1:n)
)
df$fill_code <- factor(seq_len(n^2))

p <- ggplot(df, aes(x = v, y = u, fill = fill_code)) +
  geom_tile(color = "grey60") +
  coord_equal() +
  scale_fill_vsup(n = n, mode = "usl", guide = "none") +
  theme_minimal()

lg <- vsup_legend_square_grob(
  pal_mat,
  value_breaks = c("low", "mid", "high"),
  unc_breaks   = c("low", "mid", "high"),
  value_label = "Value",
  uncertainty_label = "Uncertainty"
)

grid::grid.newpage()
grid::grid.draw(lg)

vsup_legend_fan_grob <- function(
    pal_mat,
    value_breaks = NULL,
    unc_breaks   = NULL,
    value_label  = "Value",
    uncertainty_label = "Uncertainty",
    theta_range = c(5 * pi / 6, pi / 6),
    r_outer   = 0.48,
    r_inner   = 0.10,
    arc_steps = 40
) {
  n_unc <- nrow(pal_mat)
  n_val <- ncol(pal_mat)
  if (is.null(value_breaks)) {
    value_breaks <- seq_len(n_val)
  }
  if (is.null(unc_breaks)) {
    unc_breaks <- seq_len(n_unc)
  }
  r_bound <- seq(r_outer, r_inner, length.out = n_unc + 1)
  theta_start <- theta_range[1]
  theta_end   <- theta_range[2]
  theta_bound <- seq(theta_start, theta_end, length.out = n_val + 1)
  vp <- grid::viewport(
    x = 0.5, y = 0.5,
    width = 1, height = 1,
    just = c("center", "center"),
    name = "vsup-legend-fan"
  )
  grobs <- list()
  for (ir in seq_len(n_unc)) {
    r0 <- r_bound[ir]
    r1 <- r_bound[ir + 1L]
    for (iv in seq_len(n_val)) {
      t0 <- theta_bound[iv]
      t1 <- theta_bound[iv + 1L]
      theta_outer <- seq(t0, t1, length.out = arc_steps)
      theta_inner <- seq(t1, t0, length.out = arc_steps)
      x_outer <- 0.5 + r0 * cos(theta_outer)
      y_outer <- 0.5 + r0 * sin(theta_outer)
      x_inner <- 0.5 + r1 * cos(theta_inner)
      y_inner <- 0.5 + r1 * sin(theta_inner)
      grobs[[length(grobs) + 1L]] <- grid::polygonGrob(
        x = c(x_outer, x_inner),
        y = c(y_outer, y_inner),
        default.units = "npc",
        gp = grid::gpar(
          fill = pal_mat[ir, iv],
          col  = NA
        )
      )
    }
  }
  r_tick <- r_bound[1] + 0.01
  theta_ticks <- theta_bound
  x_arc <- 0.5 + r_tick * cos(seq(theta_start, theta_end, length.out = 100))
  y_arc <- 0.5 + r_tick * sin(seq(theta_start, theta_end, length.out = 100))
  grobs[[length(grobs) + 1L]] <- grid::linesGrob(
    x = x_arc, y = y_arc,
    default.units = "npc",
    gp = grid::gpar(col = "black", lwd = 1)
  )
  tick_len <- 0.02
  for (i in seq_along(theta_ticks)) {
    th <- theta_ticks[i]
    x0 <- 0.5 + r_tick * cos(th)
    y0 <- 0.5 + r_tick * sin(th)
    x1 <- 0.5 + (r_tick + tick_len) * cos(th)
    y1 <- 0.5 + (r_tick + tick_len) * sin(th)
    grobs[[length(grobs) + 1L]] <- grid::segmentsGrob(
      x0 = x0, y0 = y0,
      x1 = x1, y1 = y1,
      default.units = "npc",
      gp = grid::gpar(col = "black", lwd = 0.7)
    )
    if (i <= length(value_breaks)) {
      xt <- 0.5 + (r_tick + 2 * tick_len) * cos(th)
      yt <- 0.5 + (r_tick + 2 * tick_len) * sin(th)
      grobs[[length(grobs) + 1L]] <- grid::textGrob(
        label = value_breaks[i],
        x = xt, y = yt,
        default.units = "npc",
        gp = grid::gpar(cex = 0.6)
      )
    }
  }
  th_u <- theta_end
  for (j in seq_len(n_unc)) {
    rj <- r_bound[j + 1L]
    x0 <- 0.5 + rj * cos(th_u)
    y0 <- 0.5 + rj * sin(th_u)
    x1 <- 0.5 + (rj - 0.02) * cos(th_u)
    y1 <- 0.5 + (rj - 0.02) * sin(th_u)
    grobs[[length(grobs) + 1L]] <- grid::segmentsGrob(
      x0 = x0, y0 = y0,
      x1 = x1, y1 = y1,
      default.units = "npc",
      gp = grid::gpar(col = "black", lwd = 0.7)
    )
    xt <- 0.5 + (rj - 0.05) * cos(th_u)
    yt <- 0.5 + (rj - 0.05) * sin(th_u)
    grobs[[length(grobs) + 1L]] <- grid::textGrob(
      label = unc_breaks[j],
      x = xt, y = yt,
      default.units = "npc",
      gp = grid::gpar(cex = 0.6)
    )
  }
  th_mid <- (theta_start + theta_end) / 2
  x_val <- 0.5 + (r_tick + 0.12) * cos(th_mid)
  y_val <- 0.5 + (r_tick + 0.12) * sin(th_mid)
  grobs[[length(grobs) + 1L]] <- grid::textGrob(
    label = value_label,
    x = x_val, y = y_val,
    default.units = "npc",
    gp = grid::gpar(cex = 0.8)
  )
  x_unc <- 0.5 + (r_inner - 0.15) * cos(th_u)
  y_unc <- 0.5 + (r_inner - 0.15) * sin(th_u)
  grobs[[length(grobs) + 1L]] <- grid::textGrob(
    label = uncertainty_label,
    x = x_unc, y = y_unc,
    default.units = "npc",
    rot = 90,
    gp = grid::gpar(cex = 0.8)
  )
  grid::grobTree(children = do.call(grid::gList, grobs), vp = vp)
}

pal_mat <- vsup_square_palette(n = 5, mode = "usl")

grid::grid.newpage()
grid::grid.draw(
  vsup_legend_fan_grob(
    pal_mat = pal_mat,
    value_breaks = c(-10, -1, 8, 16, 25, 34, 43, 51, 60)[1:ncol(pal_mat)],
    unc_breaks = 1:nrow(pal_mat),
    value_label = "Value",
    uncertainty_label = "Uncertainty"
  )
)
