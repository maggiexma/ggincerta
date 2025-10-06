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
    coord_sf(default = TRUE)
  )
}

which_vc <- function(data) {
  w <- character()
  if("fill" %in% names(data)) {
    if(is.list(data$fill)) w <- c(w, "fill")
  } else if("colour" %in% names(data)) {
    if(is.list(data$colour)) w <- c(w, "colour")
  }
  stopifnot(length(w) > 0)
  w
}

#' @export
vc <- function(v1, v2) {
  ind <- seq_along(v1)
  structure(lapply(ind, function(i) list(v1 = v1[i], v2 = v2[i])),
            class = c("bivariate", "list"))
}
