bivar_palette <- function(col1,
                          col2,
                          difC  = c(4, 4),
                          blend = c("additive", "subtractive"),
                          flip  = c("none", "vertical", "horizontal", "both")) {
  blend <- match.arg(blend)
  flip  <- match.arg(flip)

  grad1 <- colorRampPalette(c("white", col1))
  grad2 <- colorRampPalette(c("white", col2))
  dif1  <- rev(grad1(10)[1:4])
  dif2  <- rev(grad2(10)[1:4])

  ramp1 <- colorRamp(c(dif1[difC[1]], col1))
  ramp2 <- colorRamp(c(dif2[difC[2]], col2))

  lam1 <- c(0, .5, 1, 0, .5, 1, 0, .5, 1)
  lam2 <- c(0, 0, 0, .5, .5, .5, 1, 1, 1)

  m1 <- ramp1(lam1)
  m2 <- ramp2(lam2)

  if (blend == "subtractive") {
    m1r <- PBSmapping::RGB2RYB(m1)
    m2r <- PBSmapping::RGB2RYB(m2)
    m1r[is.na(m1r)] <- 0
    m2r[is.na(m2r)] <- 0
    mix <- round(PBSmapping::RYB2RGB((m1r + m2r) / 2) * 255)
  } else {
    mix <- round((m1 + m2) / 2)
  }

  cols <- apply(mix, 1, function(v)
    rgb(v[1], v[2], v[3], maxColorValue = 255))

  if (flip == "vertical")
    cols <- cols[c(9, 4, 5, 2, 3, 6, 1, 8, 7)]
  else if (flip == "horizontal")
    cols <- cols[c(7, 8, 9, 4, 5, 6, 1, 2, 3)]
  else if (flip == "both")
    cols <- cols[c(3, 2, 1, 6, 5, 4, 9, 8, 7)]

  function(n)
    cols[seq_len(min(n, length(cols)))]
}
