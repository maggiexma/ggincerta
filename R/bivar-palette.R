bivar_palette <- function(
  colors = NULL,
  n_breaks = NULL,
  blend = c("additive", "subtractive"),
  flip = c("none", "vertical", "horizontal", "both")
) {
  blend <- match.arg(blend)
  flip <- match.arg(flip)

  if (any(is.null(n_breaks))) {
    n_breaks <- c(4, 4)
  }

  # difC in Vizumap controls the ramp start (choose one in 4 lightest colors in gradient length 10)
  # Following code fixes the start to white
  # Mimic Vizumap or fix it to white?
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

  m1[ ,2] <- m1[ ,2]+10^-10
  m2[ ,2] <- m2[ ,2]+10^-10

  m1[m1>255] <- 255
  m2[m2>255] <- 255

  # more thought into blending of colors needs to be given
  # note that if n_breaks_primary != n_breaks_secondary
  # the mixing below doesn't work!!
  if (blend == "subtractive") {
    m1r <- RGB2RYB(m1)
    m2r <- RGB2RYB(m2)
    m1r[is.na(m1r)] <- 0
    m2r[is.na(m2r)] <- 0
    mix <- round(RYB2RGB(sqrt((m1r^2 + m2r^2)/2)) * 255)
    mix[is.na(mix)] <- 0
  } else {
    mix <- round((m1 + m2) / 2)
  }

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

# from PBSmapping
RYB2RGB <- function(RYBmat) {
  if (is.null(dim(RYBmat))) {
    if (length(RYBmat) > 2) {
      RYBmat <- matrix(RYBmat, ncol = 3, byrow = TRUE)
    }
  }
  if (nrow(RYBmat) == 3 && ncol(RYBmat) != 3) {
    RYBmat = t(RYBmat)
  }
  if (any(RYBmat > 1)) {
    RYBmat = RYBmat / 255
  }
  R.ryb = RYBmat[, 1]
  Y.ryb = RYBmat[, 2]
  B.ryb = RYBmat[, 3]
  I.b = pmin(R.ryb, Y.ryb, B.ryb)
  r.ryb = R.ryb - I.b
  y.ryb = Y.ryb - I.b
  b.ryb = B.ryb - I.b
  r.rgb = r.ryb + y.ryb - pmin(y.ryb, b.ryb)
  g.rgb = y.ryb + pmin(y.ryb, b.ryb)
  b.rgb = 2 * (b.ryb - pmin(y.ryb, b.ryb))
  n = pmax(r.rgb, g.rgb, b.rgb) / pmax(r.ryb, y.ryb, b.ryb)
  n[n == 0] = 1
  rp.rgb = r.rgb / n
  gp.rgb = g.rgb / n
  bp.rgb = b.rgb / n
  I.w = pmin(1 - R.ryb, 1 - Y.ryb, 1 - B.ryb)
  R.rgb = rp.rgb + I.w
  G.rgb = gp.rgb + I.w
  B.rgb = bp.rgb + I.w
  RGB = cbind(red = R.rgb, green = G.rgb, blue = B.rgb)
  return(RGB)
}

RGB2RYB <- function(RGBmat) {
  if (is.null(dim(RGBmat))) {
    if (length(RGBmat) > 2) {
      RGBmat <- matrix(RGBmat, ncol = 3, byrow = TRUE)
    }
  }
  if (nrow(RGBmat) == 3 && ncol(RGBmat) != 3) {
    RGBmat = t(RGBmat)
  }
  if (any(RGBmat > 1)) {
    RGBmat = RGBmat / 255
  }
  R.rgb = RGBmat[, 1]
  G.rgb = RGBmat[, 2]
  B.rgb = RGBmat[, 3]
  I.w = pmin(R.rgb, G.rgb, B.rgb)
  r.rgb = R.rgb - I.w
  g.rgb = G.rgb - I.w
  b.rgb = B.rgb - I.w
  r.ryb = r.rgb - pmin(r.rgb, g.rgb)
  y.ryb = 0.5 * (g.rgb + pmin(r.rgb, g.rgb))
  b.ryb = 0.5 * (b.rgb + g.rgb - pmin(r.rgb, g.rgb))
  n = pmax(r.ryb, y.ryb, b.ryb) / pmax(r.rgb, g.rgb, b.rgb)
  n[n == 0] = 1
  rp.ryb = r.ryb / n
  yp.ryb = y.ryb / n
  bp.ryb = b.ryb / n
  I.b = pmin(1 - R.rgb, 1 - G.rgb, 1 - B.rgb)
  R.ryb = rp.ryb + I.b
  Y.ryb = yp.ryb + I.b
  B.ryb = bp.ryb + I.b
  RYB = cbind(red = R.ryb, yellow = Y.ryb, blue = B.ryb)
  return(RYB)
}
