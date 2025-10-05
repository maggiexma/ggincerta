

geom_point_bivariate <- function(mapping = NULL,
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
  mapping[["fill"]] <- NA
  breaks <- match.arg(breaks)
  if(length(n_breaks) == 1L && is.numeric(n_breaks)) {
    n_breaks <- c(n_breaks, n_breaks)
  }
  stopifnot(length(n_breaks) == 2)

  c(
    layer(
      mapping = mapping,
      data = data,
      stat = stat,
      geom = "point",
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
    scale_fill_bivariate(n_breaks = n_breaks)
  )
}
