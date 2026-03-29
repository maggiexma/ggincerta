vsup_quantize <- function(v,
                          u,
                          layers = 4,
                          branch = 2L,
                          breaks = list(NULL, NULL),
                          limits = list(NULL, NULL),
                          transform = list("identity", "identity")) {
  layers <- as.integer(layers)
  branch <- as.integer(branch)

  if (!is.list(breaks)) breaks <- list(breaks)
  if (length(breaks) == 1) breaks <- rep(breaks, 2)

  if (!is.list(limits)) limits <- list(limits)
  if (length(limits) == 1) limits <- rep(limits, 2)

  if (!is.list(transform)) transform <- list(transform)
  if (length(transform) == 1) transform <- rep(transform, 2)

  v_trans <- scales::as.transform(transform[[1]])
  u_trans <- scales::as.transform(transform[[2]])

  v_t <- v_trans$transform(v)
  u_t <- u_trans$transform(u)

  max_leaf <- branch^(layers - 1L)
  layer_sizes <- branch^(0:(layers - 1L))

  v_limits_t <- if (is.null(limits[[1]])) {
    range(v_t, na.rm = TRUE, finite = TRUE)
  } else {
    sort(v_trans$transform(limits[[1]]))
  }

  u_limits_t <- if (is.null(limits[[2]])) {
    range(u_t, na.rm = TRUE, finite = TRUE)
  } else {
    sort(u_trans$transform(limits[[2]]))
  }

  v_breaks_t <- if (is.null(breaks[[1]])) {
    seq(v_limits_t[1], v_limits_t[2], length.out = max_leaf + 1L)
  } else {
    v_trans$transform(breaks[[1]])
  }

  u_breaks_t <- if (is.null(breaks[[2]])) {
    seq(u_limits_t[1], u_limits_t[2], length.out = layers + 1L)
  } else {
    u_trans$transform(breaks[[2]])
  }

  leaf_ids_by_layer <- vector("list", length = layers)
  leaf_start <- 1L
  for (ly in seq_len(layers)) {
    n_leaf <- layer_sizes[ly]
    leaf_ids_by_layer[[ly]] <- seq.int(leaf_start, length.out = n_leaf)
    leaf_start <- leaf_start + n_leaf
  }

  leaf_info <- do.call(
    rbind,
    lapply(seq_len(layers), function(ly) {
      n_leaf <- layer_sizes[ly]
      idx <- round(seq(1, max_leaf + 1, length.out = n_leaf + 1))
      layer_breaks_t <- v_breaks_t[idx]
      mids_t <- (head(layer_breaks_t, -1) + tail(layer_breaks_t, -1)) / 2

      data.frame(
        leaf = leaf_ids_by_layer[[ly]],
        layer = ly - 1L,
        v = if (n_leaf == 1L) 0.5 else (seq_len(n_leaf) - 0.5) / n_leaf,
        v_mid = v_trans$inverse(mids_t),
        stringsAsFactors = FALSE
      )
    })
  )

  out_leaf <- rep(NA_integer_, length(v))

  ok <- !(is.na(v_t) | is.na(u_t))
  ok <- ok &
    v_t >= v_limits_t[1] & v_t <= v_limits_t[2] &
    u_t >= u_limits_t[1] & u_t <= u_limits_t[2]

  if (!any(ok)) {
    return(list(
      value = factor(out_leaf, levels = leaf_info$leaf),
      leaf_info = leaf_info,
      value_breaks = if (is.null(breaks[[1]])) v_trans$inverse(v_breaks_t) else breaks[[1]],
      uncertainty_breaks = if (is.null(breaks[[2]])) u_trans$inverse(u_breaks_t) else breaks[[2]]
    ))
  }

  u_bin <- findInterval(
    u_t[ok],
    vec = u_breaks_t,
    rightmost.closed = TRUE,
    all.inside = TRUE
  )

  layer_idx0 <- layers - u_bin
  v_t_ok <- v_t[ok]
  out_ok <- integer(length(v_t_ok))

  for (ly0 in 0:(layers - 1L)) {
    idx_ok <- which(layer_idx0 == ly0)
    if (length(idx_ok) == 0L) next

    ly <- ly0 + 1L
    n_leaf <- layer_sizes[ly]

    idx_break <- round(seq(1, max_leaf + 1, length.out = n_leaf + 1))
    layer_breaks_t <- v_breaks_t[idx_break]

    bin <- findInterval(
      v_t_ok[idx_ok],
      vec = layer_breaks_t,
      rightmost.closed = TRUE,
      all.inside = TRUE
    )

    out_ok[idx_ok] <- leaf_ids_by_layer[[ly]][bin]
  }

  out_leaf[ok] <- out_ok

  list(
    value = factor(out_leaf, levels = leaf_info$leaf),
    leaf_info = leaf_info,
    value_breaks = if (is.null(breaks[[1]])) v_trans$inverse(v_breaks_t) else breaks[[1]],
    uncertainty_breaks = if (is.null(breaks[[2]])) u_trans$inverse(u_breaks_t) else breaks[[2]]
  )
}
