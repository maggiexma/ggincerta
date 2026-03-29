pal_vsup <- function(leaf_info,
                     values,
                     unc_levels = 4,
                     max_light = 0.7,
                     max_desat = 0.9,
                     pow_light = 1,
                     pow_desat = 1,
                     space = "Lab") {
  n_base <- 2^(unc_levels - 1)

  if (length(values) < 1) {
    stop("At least one colour must be provided.", call. = FALSE)
  }

  if (length(values) == 1) {
    base_cols <- colorRampPalette(c("white", values), space = space)(n_base)
  } else {
    base_cols <- colorRampPalette(values, space = space)(n_base)
  }

  ramp <- scales::colour_ramp(base_cols)

  leaf_info <- leaf_info[order(leaf_info$leaf), , drop = FALSE]

  if (unc_levels <= 1) {
    u_norm <- rep(0, nrow(leaf_info))
  } else {
    u_norm <- 1 - leaf_info$layer / (unc_levels - 1)
  }

  v_mapped <- numeric(nrow(leaf_info))
  split_idx <- split(seq_len(nrow(leaf_info)), leaf_info$layer)

  for (ly in names(split_idx)) {
    idx <- split_idx[[ly]]
    val_levels <- length(idx)

    ord <- order(leaf_info$v[idx])
    k <- integer(val_levels)
    k[ord] <- seq_len(val_levels)

    if (n_base == 1) {
      v_layer <- rep(0, val_levels)
    } else {
      v_layer <- ((k - 0.5) / val_levels - 0.5 / n_base) * n_base / (n_base - 1)
    }

    v_mapped[idx] <- v_layer
  }

  des_amt <- max_desat * u_norm^pow_desat
  light_amt <- max_light * u_norm^pow_light

  cols0 <- ramp(v_mapped)
  cols1 <- colorspace::desaturate(cols0, des_amt)

  nas <- is.na(light_amt)
  light_amt[nas] <- 0

  out <- ifelse(nas, NA, colorspace::lighten(cols1, light_amt, space = "HLS"))

  unname(as.character(out))
}
