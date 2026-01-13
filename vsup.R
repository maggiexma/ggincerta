#-------quantization
res <- compute_vsup_regular(nc$value, nc$sd)
compute_vsup_regular <- function(x, y,
                                 branch = 2,
                                 layers = 4,
                                 na_rm = TRUE) {
  browser()
  nas <- is.na(x) | is.na(y)
  if (na_rm) {
    xv <- x[!nas]
    yv <- y[!nas]
  } else {
    xv <- x
    yv <- y
  }

  v_prob <- (xv - min(xv)) / (max(xv) - min(xv))
  u_prob <- (yv - min(yv)) / (max(yv) - min(yv))

  tree <- list()
  leaf_id <- 1

  tree[[1]] <- data.frame(
    layer = 0,
    u = (layers - 1) / layers,
    v = 0.5,
    leaf = leaf_id
  )
  leaf_id <- leaf_id + 1L

  for (i in 1:(layers - 1)) {
    n_val <- 2 * branch^i
    v_cent <- seq(1, n_val - 1, by = 2) / n_val
    ui <- 1 - (i + 1)/layers

    df_i <- data.frame(
      layer = i,
      u = ui,
      v = v_cent,
      leaf = leaf_id:(leaf_id + length(v_cent) - 1L)
    )
    leaf_id <- leaf_id + nrow(df_i)
    tree[[i + 1L]] <- df_i
  }

  tree_df <- do.call(rbind, tree)
  split_layer <- split(tree_df, tree_df$layer)

  eps <- 1e-7
  assign_layer <- function(u) {
    out <- integer(length(u))
    for (k in seq_along(u)) {
      ui <- u[k]
      i <- 0L
      while (i < layers - 1L && ui < 1 - (i + 1)/layers - eps) i <- i + 1L
      out[k] <- i
    }
    out
  }

  layer_idx <- assign_layer(u_prob)

  assign_leaf <- function(v, layer) {
    out <- integer(length(v))
    uniq_layer <- unique(layer)
    for (i in uniq_layer) {
      idx <- which(layer == i)
      v_i <- v[idx]
      df_i <- split_layer[[as.character(i)]]
      v_cent <- df_i$v

      if (length(v_cent) == 1L) {
        out[idx] <- df_i$leaf
      } else {
        vgap <- (v_cent[2] - v_cent[1]) / 2
        breaks <- c(v_cent[1] - vgap,
                    (v_cent[-1] + v_cent[-length(v_cent)]) / 2,
                    v_cent[length(v_cent)] + vgap)
        bin <- cut(v_i, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        out[idx] <- df_i$leaf[bin]
      }
    }
    out
  }

  leaf_idx <- assign_leaf(v_prob, layer_idx)

  full <- rep(NA_integer_, length(x))
  full[!nas] <- leaf_idx

  list(
    value = factor(full, levels = tree_df$leaf),
    leaf_info = tree_df
  )
}

