manual_bivariate_scale <- function(
    aesthetics,
    ...,
    values,
    breaks = waiver(),
    name = waiver(),
    na.value = NA
) {
  if (rlang::is_missing(values) || is.null(values)) {
    cli::cli_abort("{.arg values} must be supplied for a manual bivariate scale.")
  }

  force(values)

  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort(
        "Insufficient values in manual bivariate scale. {n} needed but only {length(values)} provided."
      )
    }
    unname(values[seq_len(n)])
  }

  bivariate_scale(
    aesthetics = aesthetics,
    palette = pal,
    breaks = breaks,
    name = name,
    na.value = na.value,
    ...
  )
}

scale_fill_bivariate_manual <- function(
    ...,
    values,
    var1_name = NULL,
    var2_name = NULL,
    n_breaks = 4,
    bin_method = c("quantile", "equal"),
    key_size = 1.5,
    na.value = NA,
    na.translate = TRUE,
    aesthetics = "fill",
    guide = guide_bivariate()
) {
  if (length(n_breaks) == 1L && is.numeric(n_breaks)) {
    n_breaks <- rep.int(as.integer(n_breaks), 2)
  } else {
    n_breaks <- as.integer(n_breaks)
  }

  bin_method <- match.arg(bin_method)

  manual_bivariate_scale(
    aesthetics = aesthetics,
    values = values,
    guide = guide,
    na.value = na.value,
    na.translate = na.translate,
    drop = FALSE,
    n_breaks = n_breaks,
    bin_method = bin_method,
    var1_name = var1_name,
    var2_name = var2_name,
    key_size = key_size,
    ...
  )
}
