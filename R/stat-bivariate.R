#' @rdname stat-bivariate
#' @export
StatBivariate <- ggproto(
  "StatBivariate",
  StatSf,
  required_aes = c("fill|colour"),
  compute_panel = function(self,
                           data,
                           scales,
                           coord,
                           flip_axis,
                           breaks,
                           bound,
                           n_breaks,
                           colors = c("gold", "red4")) {
    if ("geometry" %in% names(data)) {
      data <- StatSf$compute_panel(data, scales, coord)
    }
    # might separate out the variables here
    w <- which_vc(data)

    compute_bivariate <- function(x, y) {
      qx <- quantile(x, seq(0, 1, length.out = n_breaks[1]), na.rm = TRUE)
      qy <- quantile(y, seq(0, 1, length.out = n_breaks[2]), na.rm = TRUE)
      if (breaks == "equal" || length(unique(qx)) < n_breaks[1])
        qx <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n_breaks[1])
      if (breaks == "equal" || length(unique(qy)) < n_breaks[2])
        qy <- seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE), length.out = n_breaks[2])
      xb <- unique(as.numeric(qx))
      yb <- unique(as.numeric(qy))

      if (!flip_axis) {
        prm_bin <- cut(x,
                       breaks = xb,
                       include.lowest = TRUE,
                       labels = FALSE)
        scd_bin <- cut(y,
                       breaks = yb,
                       include.lowest = TRUE,
                       labels = FALSE)
      } else {
        prm_bin <- cut(y,
                       breaks = yb,
                       include.lowest = TRUE,
                       labels = FALSE)
        scd_bin <- cut(x,
                       breaks = xb,
                       include.lowest = TRUE,
                       labels = FALSE)
      }

      combo <- (prm_bin - 1L) * n_breaks[2] + scd_bin
      list(value = factor(combo, levels = 1:prod(n_breaks)),
           xb = xb,
           yb = yb)
    }

    for(avar in w) {
      v1 <- sapply(data[[avar]], function(x) x$v1)
      v2 <- sapply(data[[avar]], function(x) x$v2)
      res <- compute_bivariate(v1, v2)
      pal <- bivar_palette(colors,
                           n_breaks = n_breaks,
                           blend = "additive",
                           flip = "none")

      data[[avar]] <- res$value # factor(pal[res$value], levels = pal)
      data[[paste0(avar, "_v1")]] <- v1
      data[[paste0(avar, "_v2")]] <- v2
    }
    data
  }
)
