bivar_fade_palette <- function(colours,
                               n_breaks,
                               fade = c("lighten", "alpha", "desaturate"),
                               alpha_range = c(1, 0.3),
                               max_light = 0.7,
                               max_desat = 0.9,
                               space = "Lab") {
  fade <- match.arg(fade)

  n_row <- n_breaks[1]
  n_col <- n_breaks[2]

  if (length(colours) == 1) {
    base_cols <- colorRampPalette(
      c("white", colours),
      space = space
    )(n_row)
  } else {
    base_cols <- colorRampPalette(
      colours,
      space = space
    )(n_row)
  }

  mat <- matrix(NA_character_, nrow = n_row, ncol = n_col)

  for (i in seq_len(n_row)) {
    if (fade == "alpha") {
      levs <- seq(alpha_range[1], alpha_range[2], length.out = n_col)
      mat[i, ] <- scales::alpha(base_cols[i], levs)
    } else if (fade == "lighten") {
      levs <- seq(0, max_light, length.out = n_col)
      mat[i, ] <- colorspace::lighten(
        base_cols[i],
        amount = levs,
        space = "HLS"
      )
    } else {
      levs <- seq(0, max_desat, length.out = n_col)
      mat[i, ] <- vapply(
        levs,
        function(a) colorspace::desaturate(base_cols[i], amount = a),
        character(1)
      )
    }
  }

  unname(as.vector(mat))
}
