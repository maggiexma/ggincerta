build_palette_test <- function(name, colrange = list(colour = NULL, difC = NULL), flipVertical = FALSE, flipHorizontal = FALSE, subtractive = FALSE){

  if(name == "usr"){
    if(missing(colrange))
      stop("Need to specify colours and degree of colour change when 'name = 'usr''\n")
    
    if(length(colrange$colour) != 2)
      stop("Need to specify 2 colours in colour. \n")
    
    
    isValidColour <- tryCatch ({ col2rgb(colrange$colour) }, error=function(e){ return(NULL) } )
    if (is.null(isValidColour)) {+
        stop("one of the colours specified by (", paste(colrange$colour[], collapse = " , "), ") is not a valid colour. Please
           use one of the colours from colors() or use a valid hexadecimal colour.\n")
    }
    
    for(i in 1:length(colrange$colour)) {
      # check RGB values of passed values to make sure the colour is not close to white
      rgb <- col2rgb(colrange$colour[i])
      if(length(rgb[rgb >= 200]) == 3)
        stop ("colours cannot be white or too close to white. Please select another colour from the colors() range or a hexadecimal value that is not white.")
    }
    
    if(colrange$colour[1] == colrange$colour[2])
      stop("Colours must not be the same value. Please
           use a different colour from colors() or use a different hexadecimal colour. \n")
    
    
    if (!all(colrange$difC %in% 1:4))
      stop("difC must be a vector of two elements with values spanning 1 through to 4")
    
    grad1 <- colorRampPalette(c("white", colrange$colour[1]))
    dif1 <- grad1(10)
    dif1 <- rev(dif1[1:4])
    startC1 <- dif1[colrange$difC[1]]
    
    grad2 <- colorRampPalette(c("white", colrange$colour[2]))
    dif2 <- grad2(10)
    dif2 <- rev(dif2[1:4])
    startC2 <- dif2[colrange$difC[2]]
    
    
    ramp1 <- colorRamp(c(startC1, colrange$colour[1]))
    ramp2 <- colorRamp(c(startC2, colrange$colour[2]))
    
  }
  
  # build a data frame to get light, middle, and dark colours
  # colour ramp only accepts values 0-1 (0=lightest, 1=darkest)
  lmd1 <- c(0, .5, 1, 0, .5, 1, 0, .5, 1)
  lmd2 <- c(0, 0, 0, .5, .5, .5, 1, 1, 1)
  
  lmd_df <- as.data.frame(cbind(lmd1, lmd2))
  
  # apply colour ramps
  match1 <- with(lmd_df, ramp1(lmd1))
  match2 <- with(lmd_df, ramp2(lmd2))
  
  # adjustments to conform to RGB/RYB conversion
  for(i in 1:nrow(match1)){
    match1[i,2] <- match1[i,2]+10^-10
  }
  
  for(i in 1:nrow(match2)){
    match2[i,2] <- match2[i,2]+10^-10
  }
  
  # For values greater than 255, change to 255
  match1[match1>255] <- 255
  match2[match2>255] <- 255
  
  if(subtractive){
    # convert RGB to RYB
    match1 <- RGB2RYB(match1)
    match2 <- RGB2RYB(match2)
    
    # Replace NA values with 0 (RGB2RYB white conversion)
    match1[is.na(match1)] <- 0
    match2[is.na(match2)] <- 0
    
    match_df <- as.data.frame(cbind(match1, match2))
    colnames(match_df) <- c("red1", "yellow1", "blue1", "red2", "yellow2", "blue2")
    
    # average two single hue colour palettes
    match_df$red.ave <- (match_df$red1 + match_df$red2) / 2
    match_df$yellow.ave <- (match_df$yellow1 + match_df$yellow2) / 2
    match_df$blue.ave <- (match_df$blue1 + match_df$blue2) / 2
    
    # convert average back to RGB
    match_df[,7:9] <- round(RYB2RGB(match_df[,7:9])*255)
    
    
    match_df$colour.ave <- paste(match_df$red.ave, match_df$yellow.ave, match_df$blue.ave)
    
    colours <- match_df$colour.ave
    colours <- sapply(strsplit(colours, " "), function(colours) rgb(colours[1], colours[2], colours[3], maxColorValue = 255))
  }
  else{
    match_df <- as.data.frame(cbind(match1, match2))
    colnames(match_df) <- c("red1", "green1", "blue1", "red2", "green2", "blue2")
    
    # average two single hue colour palettes
    match_df$red.ave <- round((match_df$red1 + match_df$red2) / 2)
    match_df$green.ave <- round((match_df$green1 + match_df$green2) / 2)
    match_df$blue.ave <- round((match_df$blue1 + match_df$blue2) / 2)
    
    match_df$colour.ave <- paste(match_df$red.ave, match_df$green.ave, match_df$blue.ave)
    
    colours <- match_df$colour.ave
    colours <- sapply(strsplit(colours, " "), function(colours) rgb(colours[1], colours[2], colours[3], maxColorValue = 255))
  }
  
  
  # if we flip vertically
  if(flipVertical) {
    colours <- replace(colours, c(1,9), colours[c(9, 1)]) #Switch [9] and [1]
    colours <- replace(colours, c(8,4), colours[c(4, 8)]) #switch [8] and [4]
    colours <- replace(colours, c(6,2), colours[c(2, 6)]) #Switch [6] and [2]
  }
  
  # if we flip horizontally
  if(flipHorizontal) {
    colours <- replace(colours, c(7,3), colours[c(3, 7)]) #Switch [7] and [3]
    colours <- replace(colours, c(4,2), colours[c(2, 4)]) #switch [2] and [4]
    colours <- replace(colours, c(8,6), colours[c(6, 8)]) #Switch [6] and [8]
  }
  
  oldClass(colours) <- c("palette", class(colours))
  
  colours
  
}

pal <- build_palette_test(name = "usr", colrange = list(colour = c("gold", "red4"), difC = c(4, 4)))

build_bkey_test <-
  function (data,
            palette = "BlueYellow",
            terciles = FALSE,
            flipAxis = FALSE,
            expertR_est = NA,
            expertR_err = NA,
            bound = NULL,
            fontSize = 3,
            transparent = FALSE) {
    browser()
    estimate <- names(data)[1]
    error <- names(data)[2]
    if (class(palette)[1] == "character" & length(palette) ==
        1) {
      if (palette == "BlueYellow")
        colors <- build_palette(name = "BlueYellow")
      else if (palette == "CyanMagenta")
        colors <- build_palette(name = "CyanMagenta")
      else if (palette == "BlueRed")
        colors <- build_palette(name = "BlueRed")
      else if (palette == "GreenBlue")
        colors <- build_palette(name = "GreenBlue")
      else
        stop(
          "Palette name not recognised. Must be one of BlueYellow, CyanMagenta, BlueRed or GreenBlue.\n"
        )
    } else if (class(palette)[1] == "palette") {
      colors <- palette
    } else {
      stop(
        "Palette supplied is not of class 'palette'. Please create a palette using the 'build_palette' function."
      )
    }
    if (!is.logical(flipAxis))
      stop("flipAxis must be a logical value")
    if (!is.logical(terciles))
      stop("terciles must be a logical value")
    x1 <- c(3, 4, 3, 2)
    x2 <- c(4, 5, 4, 3)
    x3 <- c(5, 6, 5, 4)
    x <- c(x1, x1 - 1, x1 - 2, x2, x2 - 1, x2 - 2, x3, x3 - 1,
           x3 - 2)
    y1 <- c(0, 1, 2, 1)
    y2 <- c(1, 2, 3, 2)
    y3 <- c(2, 3, 4, 3)
    y <- c(y1, y1 + 1, y1 + 2, y2, y2 + 1, y2 + 2, y3, y3 + 1,
           y3 + 2)
    group <- rep(1:9, each = 4)
    clr <- rep(colors, each = 4)
    
    tiles <- data.frame(
      x = x,
      y = y,
      group = group,
      color = clr
    )
    if (!flipAxis) {
      if (is.null(bound)) {
        bound <- findNbounds(
          data = data,
          estimate = estimate,
          error = error,
          terciles = terciles,
          expertR_est = expertR_est,
          expertR_err = expertR_err
        )
      }
      
      
      labels <- data.frame(
        x = c(2.5, 1.5, 0.5, -0.5, 3.5,
              4.5, 5.5, 6.5),
        y = c(-0.5, 0.5, 1.5, 2.5, -0.5,
              0.5, 1.5, 2.5),
        bound = as.character(round(bound, 2)),
        angle = c(rep(45,
                      4), rep(-45, 4))
      )
      p <- list(
        tiles = tiles,
        labels = labels,
        estimate = estimate,
        error = error,
        flipped = flipAxis,
        fontSize = fontSize,
        transparent = transparent
      )
      
    } else {
      if (is.null(bound)) {
        bound <-
          findNbounds(
            data = data,
            estimate = error,
            error = estimate,
            terciles = terciles,
            expertR_est = expertR_est,
            expertR_err = expertR_err
          )
      }
      
      labels <- data.frame(
        y = c(2.5, 1.5, 0.5, -0.5, 3.5,
              4.5, 5.5, 6.5),
        x = c(-0.5, 0.5, 1.5, 2.5, -0.5,
              0.5, 1.5, 2.5),
        bound = as.character(round(bound, 2)),
        angle = c(rep(45,
                      4), rep(-45, 4))
      )
      p <- list(
        tiles = tiles,
        labels = labels,
        estimate = error,
        error = estimate,
        flipped = flipAxis,
        fontSize = fontSize,
        transparent = transparent
      )
    }
    oldClass(p) <- c("bivkey", class(p))
    p
  }

UB_biv_key <- build_bkey_test(data = UB_dat, palette = UB_pal, terciles = TRUE, flipAxis = TRUE)

view.bivkey <- function(obj) {
  
  if (class(obj)[1] != "bivkey") {
    stop("Object is not of class 'bivkey'")
  }
  
  if (!obj$flipped) {
    x <- 'x'
    y <- 'y'
  } else {
    x <- 'y'
    y <- 'x'
  }

  if (isTRUE(obj$transparent)) {
    p <-
      ggplot() + geom_polygon(
        data = obj$tiles,
        aes_string(
          x = 'x',
          y = 'y',
          group = 'group',
          fill = 'color'
        ),
        colour = "black"
      ) +
      scale_fill_identity() +
      coord_equal() +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      geom_label(aes(
        x = -.5,
        y = .1,
        label = paste0(obj$estimate)
      ),
      angle = -45,
      size = obj$fontSize)  +
      geom_label(aes(
        x = 6.5,
        y = .1,
        label = paste0(obj$error)
      ),
      angle = 45,
      size = obj$fontSize)  +
      geom_label(
        data = obj$labels,
        aes_string(
          x = x,
          y = y,
          label = 'bound',
          angle = 'angle'
        ),
        size = obj$fontSize
      )
  } else {
    p <-
      ggplot() + geom_polygon(
        data = obj$tiles,
        aes(
          x = x,
          y = y,
          group = group,
          fill = color
        ),
        colour = "black"
      ) +
      scale_fill_identity() +
      coord_equal() +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank()
      ) +
      geom_text(aes(
        x = -.5,
        y = .1,
        label = paste0(obj$estimate)
      ),
      angle = -45,
      size = obj$fontSize)  +
      geom_text(aes(
        x = 6.5,
        y = .1,
        label = paste0(obj$error)
      ),
      angle = 45,
      size = obj$fontSize)  +
      geom_text(
        data = obj$labels,
        aes_string(
          x = x,
          y = y,
          label = 'bound',
          angle = 'angle'
        ),
        size = obj$fontSize
      )
  }
  
  p
  
}

