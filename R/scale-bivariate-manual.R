manual_bivariate_scale <- function(aesthetics,
                                   ...,
                                   values,
                                   name = waiver(),
                                   breaks = list(waiver(), waiver()),
                                   labels = list(waiver(), waiver()),
                                   limits = list(NULL, NULL),
                                   transform = list("identity", "identity"),
                                   na.value = NA,
                                   na.translate = TRUE,
                                   drop = FALSE,
                                   guide = guide_bivariate(),
                                   n_breaks = c(4, 4),
                                   bin_method = c("equal", "equal"),
                                   flip = "none",
                                   var1_name = NULL,
                                   var2_name = NULL,
                                   super = ScaleBivariate) {
  if (rlang::is_missing(values) || is.null(values)) {
    cli::cli_abort("{.arg values} must be supplied for a manual bivariate scale.")
  }

  normalize_pair <- function(x, name) {
    if (length(x) == 1)
      x <- rep(x, 2)
    if (length(x) != 2) {
      cli::cli_abort("{.arg {name}} must have length 1 or 2.")
    }
    x
  }

  n_breaks <- normalize_pair(n_breaks, "n_breaks")

  palette <- function(colours, n_breaks) {
    n_required <- prod(n_breaks)

    if (length(values) < n_required) {
      cli::cli_abort(
        "Manual bivariate scale needs {n_required} colours, but only {length(values)} provided."
      )
    }

    matrix(
      unname(values[seq_len(n_required)]),
      nrow = n_breaks[1],
      ncol = n_breaks[2],
      byrow = TRUE
    ) |> as.vector()
  }

  bivariate_scale(
    aesthetics = aesthetics,
    ...,
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    transform = transform,
    na.value = na.value,
    na.translate = na.translate,
    drop = drop,
    guide = guide,
    colors = NULL,
    palette = palette,
    n_breaks = n_breaks,
    bin_method = bin_method,
    flip = flip,
    var1_name = var1_name,
    var2_name = var2_name,
    super = super
  )
}

scale_fill_bivariate_manual <- function(...,
                                        values,
                                        name = waiver(),
                                        var1_name = NULL,
                                        var2_name = NULL,
                                        n_breaks = c(4, 4),
                                        breaks = list(waiver(), waiver()),
                                        labels = list(waiver(), waiver()),
                                        limits = list(NULL, NULL),
                                        transform = list("identity", "identity"),
                                        bin_method = c("equal", "equal"),
                                        flip = "none",
                                        na.value = NA,
                                        na.translate = TRUE,
                                        aesthetics = "fill",
                                        guide = guide_bivariate()) {
  manual_bivariate_scale(
    aesthetics = aesthetics,
    ...,
    values = values,
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    transform = transform,
    na.value = na.value,
    na.translate = na.translate,
    guide = guide,
    n_breaks = n_breaks,
    bin_method = bin_method,
    flip = flip,
    var1_name = var1_name,
    var2_name = var2_name
  )
}
