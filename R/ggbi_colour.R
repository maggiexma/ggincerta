
scale_fill_bivar <- function(colrange, subtractive = FALSE, 
                             flip_vertical = FALSE, flip_horizontal = FALSE,
                             ...) {
  
  grad1 <- grDevices::colorRampPalette(c("white", colrange$colour[1]))
  grad2 <- grDevices::colorRampPalette(c("white", colrange$colour[2]))
  
  dif1   <- rev(grad1(10)[1:4])
  dif2   <- rev(grad2(10)[1:4])

  start1 <- dif1[colrange$difC[1]]
  start2 <- dif2[colrange$difC[2]]

  ramp1  <- grDevices::colorRamp(c(start1, colrange$colour[1]))
  ramp2  <- grDevices::colorRamp(c(start2, colrange$colour[2]))
  
  lmd1 <- c(0, .5, 1, 0, .5, 1, 0, .5, 1)
  lmd2 <- c(0,   0, 0, .5, .5, .5, 1, 1, 1)
  lmd  <- data.frame(lmd1, lmd2)
  
  m1 <- ramp1(lmd$lmd1)
  m2 <- ramp2(lmd$lmd2)
  
  if (subtractive) {
    m1 <- PBSmapping::RGB2RYB(m1)
    m2 <- PBSmapping::RGB2RYB(m2)
    m1[is.na(m1)] <- 0
    m2[is.na(m2)] <- 0
    mix <- (m1 + m2) / 2
    mix <- round( PBSmapping::RYB2RGB(mix) * 255 )
  } else {
    mix <- round( (m1 + m2) / 2 )
  }
  
  cols <- apply(mix, 1, function(rgbv) {
    grDevices::rgb(rgbv[1], rgbv[2], rgbv[3], maxColorValue = 255)
  })

  if (flip_vertical) {
    cols <- cols[c(9,4,5,2,3,6,1,8,7)]
  }
  if (flip_horizontal) {
    cols <- cols[c(7,8,9,4,5,6,1,2,3)]
  }

  ggplot2::scale_fill_manual(values = cols, drop   = FALSE, 
                             guide  = 'none', ...)
}

ggplot(nc) + 
  stat_sf_bivariate(mapping = aes(estimate =  value, error = sd, 
                                  fill = after_stat(fill)), terciles = TRUE) +
  scale_fill_bivar(colrange = list(colour = c("gold", "red4"), difC = c(4,4)),
    subtractive = FALSE, flipVertical = FALSE, flipHorizontal = FALSE)

