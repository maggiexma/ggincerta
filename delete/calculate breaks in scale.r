ScaleFillBivar <- ggproto(
  "ScaleFillBivar",
  ScaleDiscrete,
  aesthetics = "fill",
  drop = FALSE,
  na.value = NA,
  
  train_df = function(self, df, ...) {
    browser()
    x <- df$estimate
    y <- df$error
    qx <- quantile(x, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE)
    qy <- quantile(y, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE)
    xb <- unique(as.numeric(qx))
    yb <- unique(as.numeric(qy))
    self$est_breaks <- xb
    self$err_breaks <- yb
    
    ggproto_parent(ScaleDiscrete, self)$train_df(df, ...)
    
    n <- as.integer((self$n_est %||% 3L) * (self$n_err %||% 3L))
    cols <- self$palette(n)
    if (length(cols) < n)
      cols <- rep_len(cols, n)
    self$legend_cols <- unname(as.character(cols))
    
    self$guide <- guide_bivariate(
      aesthetic = setNames(list(self$legend_cols), "fill"),
      value = as.character(seq_len(n)),
      label = as.character(seq_len(n)),
      est_label = format(self$est_breaks, digits = 2),
      err_label = format(self$err_breaks, digits = 2),
      est_text = self$est_text %||% "estimate",
      err_text = self$err_text %||% "error",
      size = self$guide_size
    )
    
    invisible()
  }
)
