scale_fill_bivariate <- function(data,
                                 estimate = "estimate",
                                 error = "error",
                                 colrange = list(colour = c("gold", "red4"), difC = c(4, 4)),
                                 subtractive = FALSE,
                                 flip = "none",
                                 ...) {
  est <- data[[estimate]]
  err <- data[[error]]

  est_breaks <- round(quantile(est, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE), 2)
  err_breaks <- round(quantile(err, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE), 2)

  grad1 <- grDevices::colorRampPalette(c("white", colrange$colour[1]))
  grad2 <- grDevices::colorRampPalette(c("white", colrange$colour[2]))

  dif1 <- rev(grad1(10)[1:4])
  dif2 <- rev(grad2(10)[1:4])
  ramp1 <- grDevices::colorRamp(c(dif1[colrange$difC[1]], colrange$colour[1]))
  ramp2 <- grDevices::colorRamp(c(dif2[colrange$difC[2]], colrange$colour[2]))

  lam1 <- c(0, .5, 1, 0, .5, 1, 0, .5, 1)
  lam2 <- c(0, 0, 0, .5, .5, .5, 1, 1, 1)

  m1 <- ramp1(lam1)
  m2 <- ramp2(lam2)

  if (subtractive) {
    m1 <- PBSmapping::RGB2RYB(m1)
    m2 <- PBSmapping::RGB2RYB(m2)
    m1[is.na(m1)] <- 0
    m2[is.na(m2)] <- 0
    mix <- round(PBSmapping::RYB2RGB((m1 + m2) / 2) * 255)
  } else {
    mix <- round((m1 + m2) / 2)
  }

  cols <- apply(mix, 1, function(v)
    grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255))

  if (flip == "vertical") {
    cols <- cols[c(9, 4, 5, 2, 3, 6, 1, 8, 7)]
  } else if (flip == "horizontal") {
    cols <- cols[c(7, 8, 9, 4, 5, 6, 1, 2, 3)]
  } else if (flip == "both") {
    cols <- cols[c(3, 2, 1, 6, 5, 4, 9, 8, 7)]
  }

  scale_fill_manual(
    values = cols,
    drop = FALSE,
    guide = guide_bivariate(
      aesthetic = cols,
      value = as.character(1:9),
      label = as.character(1:9),
      est_label = est_breaks,
      err_label = err_breaks,
      size = 2
    ),
    ...
  )
}
