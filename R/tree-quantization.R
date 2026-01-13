vsup_quantize <- function(v, u, layers = 4, branch = 2L, na_rm = TRUE) {
  browser()
  stopifnot(length(v) == length(u))

  nas <- is.na(v) | is.na(u)
  if (na_rm) {
    v0 <- v[!nas]
    u0 <- u[!nas]
  } else {
    v0 <- v
    u0 <- u
  }

  if (!length(v0)) {
    return(list(value = factor(integer(length(v))), leaf_info = NULL))
  }

  v_prob <- (v0 - min(v0)) / (max(v0) - min(v0) + 1e-12)
  u_prob <- (u0 - min(u0)) / (max(u0) - min(u0) + 1e-12)

  tree <- list()
  leaf_id <- 1L

  tree[[1L]] <- data.frame(
    layer = 0L,
    u = (layers - 1) / layers,
    v = 0.5,
    leaf = leaf_id
  )
  leaf_id <- leaf_id + 1L

  for (i in 1:(layers - 1L)) {
    n_val <- 2L * branch^i
    v_cent <- seq(1, n_val - 1, by = 2) / n_val
    ui <- 1 - (i + 1) / layers

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

  assign_layer <- function(u_prob) {
    out <- integer(length(u_prob))
    for (k in seq_along(u_prob)) {
      ui <- u_prob[k]
      i <- 0L
      while (i < layers - 1L && ui < 1 - (i + 1L) / layers - eps) i <- i + 1L
      out[k] <- i
    }
    out
  }

  assign_leaf <- function(v_prob, layer_idx) {
    out <- integer(length(v_prob))
    uniq_layer <- unique(layer_idx)

    for (i in uniq_layer) {
      idx <- which(layer_idx == i)
      v_i <- v_prob[idx]
      df_i <- split_layer[[as.character(i)]]
      v_cent <- df_i$v

      if (length(v_cent) == 1L) {
        out[idx] <- df_i$leaf
      } else {
        vgap   <- (v_cent[2L] - v_cent[1L]) / 2
        breaks <- c(v_cent[1L] - vgap,
                    (v_cent[-1L] + v_cent[-length(v_cent)]) / 2,
                    v_cent[length(v_cent)] + vgap)
        bin <- cut(v_i, breaks = breaks, include.lowest = TRUE,
                   labels = FALSE)
        out[idx] <- df_i$leaf[bin]
      }
    }
    out
  }

  layer_idx <- assign_layer(u_prob)
  leaf_idx <- assign_leaf(v_prob, layer_idx)

  full <- rep(NA_integer_, length(v))
  full[!nas] <- leaf_idx

  list(
    value = factor(full, levels = tree_df$leaf),
    leaf_info = tree_df
  )
}
