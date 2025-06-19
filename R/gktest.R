# internal function to build glyph for key
# have to add 5 to glyph coordinates - difference between build.glyph.map function

build.glyph.key <- function(shape) {
  
  # right now there are only two glyphs, but we can add to this
  g <- c("icone", "semi")
  if ((shape %in% g) == FALSE) {
    stop("Glyph name not recognised. Must be one of icone or semi.")
  }
  
  # create data frame of coordinates for glyph for key
  # need to add five to the y coordinates so they don't plot on top of each other in the key
  if (shape == "icone") {
    x <- seq(-3, 3, .05)
    y = (sqrt(9 - x ^ 2))
    y <- y + 5
    cir <- as.data.frame(cbind(x, y))
    x <- c(-3, 0, 3)
    y <- c(5, 0, 5)
    tri <- as.data.frame(cbind(x, y))
    glyphDat <- rbind(cir, tri)
  } else {
    x <- seq(-3, 3, .05)
    y = sqrt(9 - x ^ 2)
    y <- y + 5
    glyphDat <- as.data.frame(cbind(x, y))
  }
  
  # output glyphDat for use in the build_glyphKey function
  glyphDat
  
}

#'Build a glyph key
#'
#'This function creates a key of rotated glyphs for a map produced with
#'\code{\link{build_gmap}}.
#'
#'A key for the glyph map is not automatically generated with
#'\code{\link{build_gmap}} and must be made using \code{\link{build_gkey}}. It
#'is important that the arguments passed to \code{\link{build_gkey}} match those
#'passed to \code{\link{build_gmap}}. The map and key can be viewed together
#'using \code{\link{attach_key}}.
#'
#'@param data A data frame.
#'@param glyph Name of glyph shape. Options include \code{icone} and
#'  \code{semi}.
#'@param fontSize An integer value. Default is 3.
#'@param transparent A logical value. Option to make key background transparent.
#'  Default is FALSE.
#'
#'
#'@seealso \code{\link{attach_key}}
#'
#'
#'@examples
#'data(us_data)
#'data(us_geo)
#'co_geo <- subset(us_geo, us_geo@data$STATE == "08")
#'us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
#'co_data <- subset(us_data, us_data$GEO.id2 > 8000 & us_data$GEO.id2 < 9000)
#'co_data <- read.uv(data = co_data, estimate = "pov_rate", error = "pov_moe")
#'
#'# build a glyph key
#'key <- build_gkey(data = co_data, glyph = "icone")
#'view(key)
#'
#'@export

build_gkey <- function(data, glyph = "icone", fontSize = 3, transparent = FALSE) {
  
  nms <- names(data)
  estimate <- nms[1]
  error <- nms[2]
  
  # run internal function
  glyphDat <- build.glyph.key(shape = glyph)
  
  # create an empty data frame, which is appended after each loop iteration
  main3 <- data.frame(
    V1 = as.numeric(),
    V2 = as.numeric(),
    id = as.numeric()
  )
  
  # rotation for 0, middle, and max glyphs on key
  for (i in seq(from = 0,
                to = max(data[ ,error]),
                by = (max(data[ ,error]) / 2))) {
    theta <- (i * -pi) / max(data[ ,error])
    R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
    N <- R %*% t(glyphDat)
    N <- as.data.frame(t(N))
    id <- rep(i, nrow(glyphDat))
    final <- cbind(N, id)
    main3 <- rbind(main3, final)
  }
  
  # create an empty data frame, which is appended after each loop iteration
  extra5 <- data.frame(
    V1 = as.numeric(),
    V2 = as.numeric(),
    id = as.numeric()
  )
  
  # rotation for in-between glyphs on key
  for (i in seq(from = 0,
                to = max(data[ ,error]),
                by = (max(data[ ,error]) / 5))) {
    theta <- (i * -pi) / max(data[ ,error])
    R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
    N <- R %*% t(glyphDat)
    N <- as.data.frame(t(N))
    id <- rep(i, nrow(glyphDat))
    final <- cbind(N, id)
    extra5 <- rbind(extra5, final)
  }
  
  # create number labels for 0, middle, and max glyph
  main3_labels <-
    round(seq(
      from = 0,
      to = max(data[ ,error]),
      by = (max(data[ ,error]) / 2)
    ), 2)
  
  p <- list(main3 = main3, main3_labels = main3_labels, extra5 = extra5, key_label = paste(error), fontSize = fontSize, transparent = transparent)
  
  oldClass(p) <- c("glyphkey", class(p))
  
  p
  
}

#'@export
#'@import "ggplot2"

view.glyphkey <- function(obj) {
  if (class(obj)[1] != "glyphkey")
    stop("Object is not of class 'glyphkey'.")
  
  if (isTRUE(obj$transparent)) {
    p <- ggplot() +
      geom_polygon(
        data = obj$main3,
        aes_string(x = 'V1', y = 'V2', group = 'id'),
        fill = "black",
        alpha = .7
      ) +
      geom_polygon(
        data = obj$extra5,
        aes_string(x = 'V1', y = 'V2', group = 'id'),
        fill = "black",
        alpha = .2
      ) +
      coord_equal() +
      geom_segment(aes(
        x = -5,
        xend = 10,
        y = -0,
        yend = 0
      ),
      colour = "black",
      alpha = .5) +
      geom_segment(aes(
        x = 0,
        xend = 0,
        y = 10.5,
        yend = -10.5
      ),
      colour = "black",
      alpha = .5) +
      geom_label(aes(
        x = 0,
        y = 12,
        label = obj$key_label
      ), size = obj$fontSize) +
      geom_label(aes(
        x = -4,
        y = 7,
        label = obj$main3_labels[1]
      ), size = obj$fontSize) +
      geom_label(aes(
        x = 11.1,
        y = 1.4,
        label = obj$main3_labels[2]
      ), size = obj$fontSize) +
      geom_label(aes(
        x = -5.5,
        y = -7,
        label = obj$main3_labels[3]
      ), size = obj$fontSize) +
      xlim(c(-9, 13)) +
      ylim(c(-11, 12)) +
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
      )
  } else {
    p <- ggplot() +
      geom_polygon(
        data = obj$main3,
        aes_string(x = 'V1', y = 'V2', group = 'id'),
        fill = "black",
        alpha = .7
      ) +
      geom_polygon(
        data = obj$extra5,
        aes_string(x = 'V1', y = 'V2', group = 'id'),
        fill = "black",
        alpha = .2
      ) +
      coord_equal() +
      geom_segment(aes(
        x = -5,
        xend = 10,
        y = -0,
        yend = 0
      ),
      colour = "black",
      alpha = .5) +
      geom_segment(aes(
        x = 0,
        xend = 0,
        y = 10.5,
        yend = -10.5
      ),
      colour = "black",
      alpha = .5) +
      geom_text(aes(
        x = 0,
        y = 12,
        label = obj$key_label
      ), size = obj$fontSize) +
      geom_text(aes(
        x = -4,
        y = 7,
        label = obj$main3_labels[1]
      ), size = obj$fontSize) +
      geom_text(aes(
        x = 11.1,
        y = 1.4,
        label = obj$main3_labels[2]
      ), size = obj$fontSize) +
      geom_text(aes(
        x = -5.5,
        y = -7,
        label = obj$main3_labels[3]
      ), size = obj$fontSize) +
      xlim(c(-9, 13)) +
      ylim(c(-11, 12)) +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank()
      )
  }
  
  
  p
  
}



