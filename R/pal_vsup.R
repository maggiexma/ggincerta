pal_vsup <- function(values, unc_levels = 4, max_light = 0.9, max_desat = 0, pow_light = 0.8, pow_desat = 1) {
  n <- 2^(unc_levels - 1)
  if (length(values) != n) {
    stop(length(values), " colors are provided but ", n, " colors are needed for ", unc_levels, " uncertainty levels.", call. = FALSE)
  }

  ramp <- scales::colour_ramp(values)

  map_to_discrete <- function(v, u) {
    j <- 1 + floor((1 - u) * unc_levels)
    j <- ifelse(j >= unc_levels, unc_levels, j)

    val_levels <- 2^(j-1)
    i <- 1 + floor(v * val_levels)
    i <- ifelse( i >= val_levels, val_levels, i)

    list(i = i, j = j, v = ((i - 0.5)/val_levels - 0.5/n)*n/(n - 1), u = 1 - (j - 1)/(unc_levels - 1))
  }

  function(v, u){
    x <- map_to_discrete(v, u)
    v <- x$v
    u <- x$u

    des_amt <- max_desat*u^pow_desat
    light_amt <- max_light*u^pow_light
    cols_des <- colorspace::desaturate(ramp(v), des_amt)
    nas <- is.na(light_amt)
    light_amt[nas] <- 0
    ifelse(nas, NA, colorspace::lighten(cols_des, light_amt, space = "HLS"))
  }
}
