bivar_palette <- function(
  colors = NULL,
  n_breaks = NULL,
  flip = c("none", "vertical", "horizontal", "both")
) {
  flip <- match.arg(flip)

  if (any(is.null(n_breaks))) {
    n_breaks <- c(4, 4)
  }

  grad1 <- grDevices::colorRampPalette(c("white", colors[1]))
  grad2 <- grDevices::colorRampPalette(c("white", colors[2]))
  dif1 <- rev(grad1(round(n_breaks[1] * 2.5))[1:n_breaks[1]])
  dif2 <- rev(grad2(round(n_breaks[2] * 2.5))[1:n_breaks[2]])

  ramp1 <- grDevices::colorRamp(c(dif1[n_breaks[1]], colors[1]))
  ramp2 <- grDevices::colorRamp(c(dif2[n_breaks[2]], colors[2]))

  lam1 <- rep(seq(0, 1, length.out = n_breaks[1]), times = n_breaks[2])
  lam2 <- rep(seq(0, 1, length.out = n_breaks[2]), each = n_breaks[1])

  m1 <- ramp1(lam1)
  m2 <- ramp2(lam2)

  mix <- round((m1 + m2) / 2)

  cols <- apply(mix, 1, function(v) {
    grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
  })

  n <- prod(n_breaks)
  n1 <- n_breaks[1]
  n2 <- n_breaks[2]
  mat <- matrix(1:n, nrow = n1, ncol = n2, byrow = FALSE)
  cols <- switch(
    flip,
    "vertical" = cols[as.vector(t(mat[n1:1, n2:1]))],
    "horizontal" = cols[as.vector(t(mat))],
    "both" = cols[mat[n1:1, n2:1]],
    cols
  )
  cols[seq_len(min(n, length(cols)))]
}
