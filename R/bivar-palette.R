bivar_palette <- function(color_primary,
                          color_secondary,
                          n_breaks_primary = 4,
                          n_breaks_secondary = 4,
                          blend = c("additive", "subtractive")#,
                          # remove flip for now
                          #flip  = c("none", "vertical", "horizontal", "both")
                          ) {
  blend <- match.arg(blend)
  #flip  <- match.arg(flip)

  grad1 <- colorRampPalette(c("white", color_primary))
  grad2 <- colorRampPalette(c("white", color_secondary))
  dif1  <- rev(grad1(round(n_breaks_primary * 2.5))[1:n_breaks_primary])
  dif2  <- rev(grad2(round(n_breaks_secondary * 2.5))[1:n_breaks_secondary])

  ramp1 <- colorRamp(c(dif1[n_breaks_primary], color_primary))
  ramp2 <- colorRamp(c(dif2[n_breaks_secondary], color_secondary))

  lam1 <- rep(seq(0, 1, length.out = n_breaks_primary - 1), times = n_breaks_secondary)
  lam2 <- rep(seq(0, 1, length.out = n_breaks_secondary - 1), each = n_breaks_primary)

  m1 <- ramp1(lam1)
  m2 <- ramp2(lam2)

  # more thought into blending of colors needs to be given
  # note that if n_breaks_primary != n_breaks_secondary
  # the mixing below doesn't work!!
  if (blend == "subtractive") {
    m1r <- RGB2RYB(m1)
    m2r <- RGB2RYB(m2)
    m1r[is.na(m1r)] <- 0
    m2r[is.na(m2r)] <- 0
    mix <- round(RYB2RGB((m1r + m2r) / 2) * 255)
  } else {
    mix <- round((m1 + m2) / 2)
  }

  cols <- apply(mix, 1, function(v)
    rgb(v[1], v[2], v[3], maxColorValue = 255))

  ## remove flip for now
  # if (flip == "vertical")
  #   cols <- cols[c(9, 4, 5, 2, 3, 6, 1, 8, 7)]
  # else if (flip == "horizontal")
  #   cols <- cols[c(7, 8, 9, 4, 5, 6, 1, 2, 3)]
  # else if (flip == "both")
  #   cols <- cols[c(3, 2, 1, 6, 5, 4, 9, 8, 7)]

  function(n)
    cols[seq_len(min(n, length(cols)))]
}


# from PBSmapping
RYB2RGB <- function (RYBmat) {
  if (is.null(dim(RYBmat)))
    if (length(RYBmat) > 2)
      RYBmat <- matrix(RYBmat, ncol = 3, byrow = TRUE)
  if (nrow(RYBmat) == 3 && ncol(RYBmat) != 3)
    RYBmat = t(RYBmat)
  if (any(RYBmat > 1))
    RYBmat = RYBmat/255
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
  n = pmax(r.rgb, g.rgb, b.rgb)/pmax(r.ryb, y.ryb, b.ryb)
  n[n == 0] = 1
  rp.rgb = r.rgb/n
  gp.rgb = g.rgb/n
  bp.rgb = b.rgb/n
  I.w = pmin(1 - R.ryb, 1 - Y.ryb, 1 - B.ryb)
  R.rgb = rp.rgb + I.w
  G.rgb = gp.rgb + I.w
  B.rgb = bp.rgb + I.w
  RGB = cbind(red = R.rgb, green = G.rgb, blue = B.rgb)
  return(RGB)
}

RGB2RYB <- function (RGBmat) {
  if (is.null(dim(RGBmat)))
    if (length(RGBmat) > 2)
      RGBmat <- matrix(RGBmat, ncol = 3, byrow = TRUE)
  if (nrow(RGBmat) == 3 && ncol(RGBmat) != 3)
    RGBmat = t(RGBmat)
  if (any(RGBmat > 1))
    RGBmat = RGBmat/255
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
  n = pmax(r.ryb, y.ryb, b.ryb)/pmax(r.rgb, g.rgb, b.rgb)
  n[n == 0] = 1
  rp.ryb = r.ryb/n
  yp.ryb = y.ryb/n
  bp.ryb = b.ryb/n
  I.b = pmin(1 - R.rgb, 1 - G.rgb, 1 - B.rgb)
  R.ryb = rp.ryb + I.b
  Y.ryb = yp.ryb + I.b
  B.ryb = bp.ryb + I.b
  RYB = cbind(red = R.ryb, yellow = Y.ryb, blue = B.ryb)
  return(RYB)
}
