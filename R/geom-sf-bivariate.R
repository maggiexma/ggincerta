StatBivariate <- ggproto(
  "StatBivariate",
  Stat,
  required_aes = c("estimate", "error"),
  compute_panel = function(data,
                           scales,
                           coord,
                           terciles = TRUE,
                           flipAxis = FALSE,
                           bound = NULL) {
    if ("geometry" %in% names(data)) {
      data <- StatSf$compute_panel(data, scales, coord)
    }

    x <- data$estimate
    y <- data$error
    qx <- quantile(x, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE)
    qy <- quantile(y, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE)
    if (!terciles ||
        length(unique(qx)) < 4)
      qx <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 4)
    if (!terciles ||
        length(unique(qy)) < 4)
      qy <- seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE), length.out = 4)
    xb <- unique(as.numeric(qx))
    yb <- unique(as.numeric(qy))

    if (!flipAxis) {
      est_bin <- cut(x,
                     breaks = xb,
                     include.lowest = TRUE,
                     labels = FALSE)
      err_bin <- cut(y,
                     breaks = yb,
                     include.lowest = TRUE,
                     labels = FALSE)
    } else {
      est_bin <- cut(y,
                     breaks = yb,
                     include.lowest = TRUE,
                     labels = FALSE)
      err_bin <- cut(x,
                     breaks = xb,
                     include.lowest = TRUE,
                     labels = FALSE)
    }

    n_est <- 3L
    n_err <- 3L
    combo <- (est_bin - 1L) * n_err + err_bin
    data$fill <- factor(combo, levels = 1:(n_est * n_err))

    attr(data$fill, "bivar_breaks") <- list(
      est_breaks = xb,
      err_breaks = yb,
      n_est = n_est,
      n_err = n_err
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
#' @param terciles Logical; whether to use tercile breaks.
#' @param flipAxis Logical; whether to flip estimate/error axes.
#' @param ... Additional arguments passed to \code{layer_sf()}.
#'
#' @examples
#' set.seed(10086)
#'
#' data(nc)
#'
#' ggplot(nc) +
#'   geom_sf_bivariate(aes(estimate = value, error = sd)) +
#'   scale_fill_bivariate(
#'     colrange = list(colour = c("gold", "red4"), difC = c(4, 4)),
#'     subtractive = FALSE,
#'     flip_vertical = FALSE,
#'     flip_horizontal = FALSE
#'   )
#'
#' @return A ggproto object rendering the filled sf layer and a \code{coord_sf()}.
#'
#' @import ggplot2
#' @export
geom_sf_bivariate <- function(mapping = NULL,
                              data = NULL,
                              stat = StatBivariate,
                              position = "identity",
                              show.legend = TRUE,
                              inherit.aes = TRUE,
                              na.rm = FALSE,
                              terciles = TRUE,
                              flipAxis = FALSE,
                              ...) {
  if (is.null(mapping))
    mapping <- aes()
  if (is.null(mapping[["fill"]])) {
    mapping[["fill"]] <- rlang::expr(after_stat(fill))
  }

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
        terciles = terciles,
        flipAxis = flipAxis,
        coord = coord_sf(),
        ...
      )
    ),
    coord_sf()
  )
}
