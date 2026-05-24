# Copied and adapted from ggChernoff by David Selby.
# Fix midpoint handling by moving it to the rescaler.
# Reverse the mapping between smile and frown.
scale_smile_continuous <- function(...,
                                   range = c(1, -1),
                                   midpoint = mean,
                                   na.value = 1) {
  if (is.numeric(midpoint)) {
    mid_fun <- function(x, ...) midpoint
  } else {
    mid_fun <- match.fun(midpoint)
  }

  ggplot2::continuous_scale(
    aesthetics = "smile",
    palette = scales::identity_pal(),
    rescaler = function(x,
                        to = range,
                        from = base::range(x, na.rm = TRUE)) {
      scales::rescale_mid(
        x,
        to = to,
        from = from,
        mid = mid_fun(x, na.rm = TRUE)
      )
    },
    ...,
    na.value = na.value
  )
}

scale_smile <- scale_smile_continuous
