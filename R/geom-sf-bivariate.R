#' @rdname stat-bivariate
#' @export
StatBivariate <- ggproto(
  "StatBivariate",
  StatSf,
  required_aes = c("v1", "v2"),
  compute_panel = function(data,
                           scales,
                           coord,
                           flip_axis,
                           breaks,
                           bound,
                           n_breaks) {
    if ("geometry" %in% names(data)) {
      data <- StatSf$compute_panel(data, scales, coord)
    }

    x <- data$v1
    y <- data$v2
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
    data$fill <- factor(combo, levels = 1:prod(n_breaks))

    attr(data$fill, "bivar_breaks") <- list(
      prm_breaks = xb,
      scd_breaks = yb,
      n_prm = n_breaks[1],
      n_scd = n_breaks[2]
    )
    data
  }
)

#' @title Geom layer function for bivariate map on sf objects
#' @description
#' geom_sf_bivariate applies StatBivariate to sf data, classifying each region
#' into joint estimate/error bins and returning a map with bivariate colour scheme.
#'
#' @param mapping Aesthetic mapping; must include \code{geometry}, \code{estimate}, and \code{error}.
#' @param data An sf object.
#' @param geom Geom type to use.
#' @param position Position adjustment.
#' @param show.legend Logical; whether to display a legend.
#' @param inherit.aes Logical; whether to inherit global aesthetics.
#' @param breaks Whether to use "quantile" or "equal" bins.
#' @param n_breaks The number of breaks
#' @param flip_axis Logical; whether to flip estimate/error axes.
#' @param ... Additional arguments passed to \code{layer_sf()}.
#'
#' @examples
#' set.seed(10086)
#' data(nc)
#' ggplot(nc) +
#'   geom_sf_bivariate(aes(v1 = value, v2 = sd))
#'
#' @return A ggproto object rendering the filled sf layer and a \code{coord_sf()}.
#' @export
geom_sf_bivariate <- function(mapping = NULL,
                              data = NULL,
                              stat = StatBivariate,
                              position = "identity",
                              show.legend = TRUE,
                              inherit.aes = TRUE,
                              na.rm = FALSE,
                              flip_axis  = FALSE,
                              breaks = c("quantile", "equal"),
                              n_breaks = 3L,
                              ...) {
  if (is.null(mapping))
    mapping <- aes()
  if (is.null(mapping[["fill"]])) {
    mapping[["fill"]] <- rlang::expr(after_stat(fill))
  }
  breaks <- match.arg(breaks)
  if(length(n_breaks) == 1L && is.numeric(n_breaks)) {
    n_breaks <- c(n_breaks, n_breaks)
  }
  stopifnot(length(n_breaks) == 2)

  c(
    layer_sf(
      mapping = mapping,
      data = data,
      stat = stat,
      geom = "sf",
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        flip_axis = flip_axis,
        breaks = breaks,
        n_breaks = n_breaks,
        ...
      )
    ),
    coord_sf(),
    scale_fill_bivariate(n_breaks = n_breaks)
  )
}
