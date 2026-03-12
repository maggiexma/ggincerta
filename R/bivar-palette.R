bivar_palette <- function(colors = NULL,
                          n_breaks = c(4, 4),
                          flip = c("none", "vertical", "horizontal", "both")) {
  flip <- match.arg(flip)

  n_x <- n_breaks[1]
  n_y <- n_breaks[2]

  grad1 <- colorRampPalette(c("white", colors[1]))
  grad2 <- colorRampPalette(c("white", colors[2]))

  dif1 <- rev(grad1(round(n_x * 2.5))[1:n_x])
  dif2 <- rev(grad2(round(n_y * 2.5))[1:n_y])

  ramp1 <- colorRamp(c(dif1[n_x], colors[1]))
  ramp2 <- colorRamp(c(dif2[n_y], colors[2]))

  lam1 <- rep(seq(0, 1, length.out = n_x), times = n_y)
  lam2 <- rep(seq(0, 1, length.out = n_y), each = n_x)

  m1 <- ramp1(lam1)
  m2 <- ramp2(lam2)

  mix <- round((m1 + m2) / 2)

  cols <- apply(mix, 1, function(v) {
    rgb(v[1], v[2], v[3], maxColorValue = 255)
  })

  idx <- matrix(seq_len(n_x * n_y),
                nrow = n_y,
                ncol = n_x,
                byrow = TRUE)

  cols <- switch(
    flip,
    "vertical" = cols[as.vector(idx[n_y:1, ])],
    "horizontal" = cols[as.vector(idx[, n_x:1])],
    "both" = cols[as.vector(idx[n_y:1, n_x:1])],
    cols
  )

  unname(cols)
}
