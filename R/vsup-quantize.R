#' Tree quantization for VSUPs
#'
#' Quantize value and uncertainty variables into a hierarchical VSUP tree.
#'
#' @inheritParams scale_fill_vsup
#' @param v Numeric vector of value variable.
#' @param u Numeric vector of uncertainty variable.
#'
#' @return
#' A list containing quantized leaf ids and break information.
#'
#' @export
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

  clean_breaks <- function(x) {
    x <- sort(unique(as.numeric(x)))
    x[is.finite(x)]
  }

  range_finite <- function(x) {
    x <- x[is.finite(x)]
    if (length(x)) range(x) else c(0, 1)
  }

  v_limits_raw <- if (is.null(limits[[1]])) {
    range_finite(v)
  } else {
    sort(as.numeric(limits[[1]]))
  }

  u_limits_raw <- if (is.null(limits[[2]])) {
    range_finite(u)
  } else {
    sort(as.numeric(limits[[2]]))
  }

  v_limits_t <- clean_breaks(v_trans$transform(v_limits_raw))
  u_limits_t <- clean_breaks(u_trans$transform(u_limits_raw))

  if (length(v_limits_t) < 2L) v_limits_t <- c(0, 1)
  if (length(u_limits_t) < 2L) u_limits_t <- c(0, 1)

  v_breaks_raw <- if (is.null(breaks[[1]])) {
    as.numeric(v_trans$inverse(
      seq(v_limits_t[1], v_limits_t[2], length.out = max_leaf + 1L)
    ))
  } else {
    clean_breaks(breaks[[1]])
  }

  u_breaks_raw <- if (is.null(breaks[[2]])) {
    as.numeric(u_trans$inverse(
      seq(u_limits_t[1], u_limits_t[2], length.out = layers + 1L)
    ))
  } else {
    clean_breaks(breaks[[2]])
  }

  v_breaks_raw <- v_breaks_raw[
    v_breaks_raw >= v_limits_raw[1] & v_breaks_raw <= v_limits_raw[2]
  ]

  u_breaks_raw <- u_breaks_raw[
    u_breaks_raw >= u_limits_raw[1] & u_breaks_raw <= u_limits_raw[2]
  ]

  v_breaks_t <- clean_breaks(v_trans$transform(v_breaks_raw))
  u_breaks_t <- clean_breaks(u_trans$transform(u_breaks_raw))

  if (length(v_breaks_t) != max_leaf + 1L) {
    cli::cli_abort(
      "{.arg breaks[[1]]} must have length {.val {max_leaf + 1L}} after applying {.arg limits}."
    )
  }

  if (length(u_breaks_t) != layers + 1L) {
    cli::cli_abort(
      "{.arg breaks[[2]]} must have length {.val {layers + 1L}} after applying {.arg limits}."
    )
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
      idx <- round(seq(1, max_leaf + 1, length.out = n_leaf + 1L))
      layer_breaks_t <- v_breaks_t[idx]
      mids_t <- (utils::head(layer_breaks_t, -1L) + utils::tail(layer_breaks_t, -1L)) / 2

      data.frame(
        leaf = leaf_ids_by_layer[[ly]],
        layer = ly - 1L,
        v = if (n_leaf == 1L) 0.5 else (seq_len(n_leaf) - 0.5) / n_leaf,
        v_mid = as.numeric(v_trans$inverse(mids_t)),
        stringsAsFactors = FALSE
      )
    })
  )

  out_leaf <- rep(NA_integer_, length(v))

  ok <- is.finite(v_t) & is.finite(u_t) &
    v_t >= v_limits_t[1] & v_t <= v_limits_t[2] &
    u_t >= u_limits_t[1] & u_t <= u_limits_t[2]

  if (any(ok)) {
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
      if (!length(idx_ok)) next

      ly <- ly0 + 1L
      n_leaf <- layer_sizes[ly]

      idx_break <- round(seq(1, max_leaf + 1, length.out = n_leaf + 1L))
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
  }

  list(
    value = factor(out_leaf, levels = leaf_info$leaf),
    leaf_info = leaf_info,
    value_breaks = v_breaks_raw,
    uncertainty_breaks = u_breaks_raw
  )
}
