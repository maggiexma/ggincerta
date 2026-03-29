#' @rdname scale_bivariate
#' @export
ScaleBivariate <- ggproto(
  "ScaleBivariate",
  ScaleDiscrete,

  drop = FALSE,
  na.value = NA,

  transform = function(self, x) {
    if (!inherits(x, "bivariate")) {
      return(x)
    }

    trans1 <- scales::as.transform(self$transforms[[1]])
    trans2 <- scales::as.transform(self$transforms[[2]])

    x1 <- vapply(x, `[[`, numeric(1), "v1")
    x2 <- vapply(x, `[[`, numeric(1), "v2")
    vars <- attr(x, "vars")

    x1_t <- trans1$transform(x1)
    x2_t <- trans2$transform(x2)

    self$.trained_values_raw <- list(x1, x2)
    self$.trained_values_transformed <- list(x1_t, x2_t)
    self$var_names <- vars

    structure(
      Map(function(a, b) list(v1 = a, v2 = b), x1_t, x2_t),
      class = c("bivariate", "list"),
      vars = vars
    )
  },

  get_limits_1d = function(self, i) {
    br <- self$breaks[[i]]
    lim <- self$limits[[i]]

    if (is.numeric(br)) {
      br <- sort(unique(as.numeric(br)))
      return(range(br[is.finite(br)]))
    }

    if (!is.null(lim)) {
      return(sort(as.numeric(lim)))
    }

    x <- self$.trained_values_raw[[i]]
    x <- x[is.finite(x)]

    if (length(x)) range(x) else c(0, 1)
  },

  get_limits = function(self) {
    list(self$get_limits_1d(1), self$get_limits_1d(2))
  },

  get_breaks_1d = function(self, i, limits = self$get_limits()[[i]]) {
    numeric_breaks <- function(x) {
      x <- sort(unique(as.numeric(x)))
      x[is.finite(x)]
    }

    br <- self$breaks[[i]]
    if (is.numeric(br)) {
      return(numeric_breaks(br))
    }

    trans <- scales::as.transform(self$transforms[[i]])
    x_raw <- self$.trained_values_raw[[i]]
    x_t <- self$.trained_values_transformed[[i]]

    keep <- is.finite(x_raw) & is.finite(x_t)
    x_raw <- x_raw[keep]
    x_t <- x_t[keep]

    if (!is.null(limits)) {
      x_t <- x_t[x_raw >= limits[1] & x_raw <= limits[2]]
    }

    limits_t <- sort(as.numeric(trans$transform(limits)))

    if (!length(x_t)) {
      br_t <- seq(limits_t[1], limits_t[2], length.out = self$n_breaks[i] + 1)
      return(as.numeric(trans$inverse(br_t)))
    }

    if (identical(self$bin_method[[i]], "quantile")) {
      br_t <- quantile(
        x_t,
        probs = seq(0, 1, length.out = self$n_breaks[i] + 1),
        na.rm = TRUE,
        names = FALSE
      )

      if (length(unique(br_t)) < length(br_t)) {
        br_t <- seq(limits_t[1], limits_t[2], length.out = self$n_breaks[i] + 1)
      } else {
        br_t[c(1, length(br_t))] <- limits_t
      }
    } else {
      br_t <- seq(limits_t[1], limits_t[2], length.out = self$n_breaks[i] + 1)
    }

    as.numeric(trans$inverse(br_t))
  },

  get_breaks = function(self, limits = self$get_limits()) {
    list(
      self$get_breaks_1d(1, limits[[1]]),
      self$get_breaks_1d(2, limits[[2]])
    )
  },

  get_breaks_transformed = function(self, limits = self$get_limits()) {
    breaks <- self$get_breaks(limits)

    list(
      scales::as.transform(self$transforms[[1]])$transform(breaks[[1]]),
      scales::as.transform(self$transforms[[2]])$transform(breaks[[2]])
    )
  },

  get_labels_1d = function(self, i, breaks) {
    labels <- self$labels[[i]]

    if (is.null(labels)) return(NULL)
    if (is_waiver(labels)) return(scales::label_number()(breaks))
    if (is.function(labels)) return(labels(breaks))
    labels
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    list(
      self$get_labels_1d(1, breaks[[1]]),
      self$get_labels_1d(2, breaks[[2]])
    )
  },

  get_key_colours = function(self, breaks = self$get_breaks()) {
    n_breaks <- c(length(breaks[[1]]) - 1L, length(breaks[[2]]) - 1L)
    self$palette_fn(n_breaks)
  },

  map = function(self, x, limits = self$get_limits()) {
    if (!inherits(x, "bivariate")) {
      return(x)
    }

    x1 <- vapply(x, `[[`, numeric(1), "v1")
    x2 <- vapply(x, `[[`, numeric(1), "v2")

    breaks_t <- self$get_breaks_transformed(limits)
    x_bin <- cut(x1, breaks_t[[1]], include.lowest = TRUE, labels = FALSE)
    y_bin <- cut(x2, breaks_t[[2]], include.lowest = TRUE, labels = FALSE)

    nx <- length(breaks_t[[1]]) - 1L
    id <- (y_bin - 1L) * nx + x_bin

    colours <- self$get_key_colours(self$get_breaks(limits))
    out <- rep(self$na.value, length(id))
    out[!is.na(id)] <- colours[id[!is.na(id)]]
    out
  },

  break_info = function(self, limits = self$get_limits()) {
    breaks <- self$get_breaks(limits)
    labels <- self$get_labels(breaks)

    list(
      title = self$name,
      limits = limits,
      n_breaks = c(length(breaks[[1]]) - 1L, length(breaks[[2]]) - 1L),
      x_breaks = breaks[[1]],
      y_breaks = breaks[[2]],
      x_labels = labels[[1]],
      y_labels = labels[[2]],
      key_colours = unname(as.character(self$get_key_colours(breaks))),
      var1_title = self$var1_name %||% self$var_names[1],
      var2_title = self$var2_name %||% self$var_names[2],
      aesthetics = self$aesthetics
    )
  },

  get_guide_info = function(self) {
    self$break_info()
  },

  train = function(self, x) invisible(),
  train_df = function(self, df) invisible()
)

bivariate_scale <- function(
    aesthetics,
    ...,
    name = waiver(),
    breaks = list(waiver(), waiver()),
    labels = list(waiver(), waiver()),
    limits = list(NULL, NULL),
    transform = list("identity", "identity"),
    na.value = NA,
    na.translate = TRUE,
    drop = FALSE,
    guide = waiver(),
    colors = c("gold", "red4"),
    palette = NULL,
    palette_params = list(),
    n_breaks = c(4, 4),
    bin_method = c("equal", "equal"),
    flip = "none",
    var1_name = NULL,
    var2_name = NULL,
    super = ScaleBivariate
) {
  normalize_pair <- function(x, name) {
    if (length(x) == 1) x <- rep(x, 2)
    if (length(x) != 2) {
      cli::cli_abort("{.arg {name}} must have length 1 or 2.")
    }
    x
  }

  normalize_pair_list <- function(x, name) {
    if (!is.list(x)) x <- list(x)
    if (length(x) == 1) x <- rep(x, 2)
    if (length(x) != 2) {
      cli::cli_abort("{.arg {name}} must have length 1 or 2.")
    }
    x
  }

  resolve_palette <- function(palette, colors, flip, palette_params) {
    if (is.null(palette)) {
      return(function(n_breaks) {
        bivar_palette(
          colors = colors,
          n_breaks = n_breaks,
          flip = flip
        )
      })
    }

    if (!is.function(palette)) {
      cli::cli_abort("{.arg palette} must be NULL or a function.")
    }

    function(n_breaks) {
      do.call(
        palette,
        c(
          list(colours = colors, n_breaks = n_breaks),
          palette_params
        )
      )
    }
  }

  n_breaks <- normalize_pair(n_breaks, "n_breaks")
  bin_method <- normalize_pair(
    match.arg(bin_method, c("quantile", "equal"), several.ok = TRUE),
    "bin_method"
  )
  transform <- normalize_pair_list(transform, "transform")
  breaks <- normalize_pair_list(breaks, "breaks")
  labels <- normalize_pair_list(labels, "labels")
  limits <- normalize_pair_list(limits, "limits")

  invisible(lapply(transform, scales::as.transform))

  sc <- discrete_scale(
    aesthetics = aesthetics,
    palette = function(n) seq_len(n),
    name = name,
    na.value = na.value,
    na.translate = na.translate,
    drop = drop,
    guide = guide,
    ...,
    super = super
  )

  sc$breaks <- breaks
  sc$labels <- labels
  sc$limits <- limits
  sc$colors <- colors
  sc$n_breaks <- n_breaks
  sc$bin_method <- bin_method
  sc$flip <- flip
  sc$transforms <- transform
  sc$var1_name <- var1_name
  sc$var2_name <- var2_name
  sc$palette_fn <- resolve_palette(
    palette = palette,
    colors = colors,
    flip = flip,
    palette_params = palette_params
  )

  sc
}

#' Bivariate color scales
#'
#' `scale_*_bivariate` creates a bivariate palette by mixing two colour ramps,
#' then implements the mapping by binning the variables `v1`, `v2` and assigning
#' each bin combination to a colour.
#'
#' @inheritParams ggplot2::discrete_scale
#' @param name1,name2 Optional names for `v1` and `v2`. Used as axis titles in
#'   the legend. If `NULL`, the default, the names are taken from the mapping.
#' @param colors A character vector of length two specifying the colors for the
#'   bivariate palette.
#' @param n_breaks An integer guiding the number of bins for each variable.
#' @param breaks Method used to bin the variables: `"quantile"` (the default)
#'   or `"equal"`.
#' @param flip Method used to flip the legend: `"none"` (the default),
#'   `"vertical"`, `"horizontal"`, or `"both"`.
#' @param guide_size A numeric value controlling the size of the legend graphic,
#'   in centimeters.
#' @param ... Other arguments passed to [ggplot2::discrete_scale()].
#'
#' @returns A `ScaleBivariate` ggproto object.
#'
#' @examples
#' # Create a bivariate fill scale
#' sc <- scale_fill_bivariate()
#' class(sc)
#' sc$palette(9)
#'
#' # Basic bivariate map
#' p <- ggplot(nc) + geom_sf(aes(fill = duo(value, sd)))
#'
#' # Customize axis labels
#' p + scale_fill_bivariate(name1 = 'var1', name2 = 'var2')
#'
#' @seealso [ggplot2::Scale] for the base `ggproto` class that all scale objects
#'   inherit from.
#'
#' @rdname scale_bivariate
#' @export
scale_fill_bivariate <- function(
    ...,
    name = waiver(),
    var1_name = NULL,
    var2_name = NULL,
    colors = c("gold", "red4"),
    palette = NULL,
    palette_params = list(),
    n_breaks = c(4, 4),
    breaks = list(waiver(), waiver()),
    labels = list(waiver(), waiver()),
    limits = list(NULL, NULL),
    transform = list("identity", "identity"),
    bin_method = c("equal", "equal"),
    flip = c("none", "vertical", "horizontal", "both"),
    na.value = NA,
    aesthetics = "fill",
    guide = guide_bivariate()
) {
  bivariate_scale(
    aesthetics = aesthetics,
    ...,
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    transform = transform,
    na.value = na.value,
    guide = guide,
    colors = colors,
    palette = palette,
    palette_params = palette_params,
    n_breaks = n_breaks,
    bin_method = bin_method,
    flip = match.arg(flip),
    var1_name = var1_name,
    var2_name = var2_name
  )
}

#' @rdname scale_bivariate
#' @export
scale_color_bivariate <- function(...,
                                  name = waiver(),
                                  var1_name = NULL,
                                  var2_name = NULL,
                                  colors = c("gold", "red4"),
                                  palette = NULL,
                                  palette_params = list(),
                                  n_breaks = c(4, 4),
                                  breaks = list(waiver(), waiver()),
                                  labels = list(waiver(), waiver()),
                                  limits = list(NULL, NULL),
                                  transform = list("identity", "identity"),
                                  bin_method = c("equal", "equal"),
                                  flip = c("none", "vertical", "horizontal", "both"),
                                  na.value = NA,
                                  aesthetics = "fill",
                                  guide = guide_bivariate()) {
  scale_fill_bivariate(
    name = name,
    var1_name = var1_name,
    var2_name = var2_name,
    colors = colors,
    palette = palette,
    palette_params = palette_params,
    n_breaks = n_breaks,
    breaks = breaks,
    labels = labels,
    limits = limits,
    transform = transform,
    bin_method = bin_method,
    flip = flip,
    na.value = na.value,
    aesthetics = aesthetics,
    guide = guide,
    ...
  )
}

#' @rdname scale_bivariate
#' @export
scale_colour_bivariate <- scale_color_bivariate

#' @export
scale_type.bivariate <- function(x) "bivariate"
