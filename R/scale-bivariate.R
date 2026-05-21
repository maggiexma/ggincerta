#' @rdname bivariate_scale
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
      Map(function(a, b)
        list(v1 = a, v2 = b), x1_t, x2_t),
      class = c("bivariate", "list"),
      vars = vars
    )
  },

  get_limits_1d = function(self, i) {
    br <- self$breaks[[i]]
    lim <- self$limits[[i]]

    if (!is.null(lim)) {
      return(sort(as.numeric(lim)))
    }

    if (is.numeric(br)) {
      br <- sort(unique(as.numeric(br)))
      br <- br[is.finite(br)]
      return(range(br))
    }

    x <- self$.trained_values_raw[[i]]
    x <- x[is.finite(x)]

    if (length(x)) {
      range(x)
    } else {
      c(0, 1)
    }
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
      br <- numeric_breaks(br)

      if (!is.null(limits)) {
        br <- br[br >= limits[1] & br <= limits[2]]
      }

      return(br)
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
    list(self$get_breaks_1d(1, limits[[1]]),
         self$get_breaks_1d(2, limits[[2]]))
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

    if (is.null(labels))
      return(NULL)
    if (is_waiver(labels))
      return(scales::label_number()(breaks))
    if (is.function(labels))
      return(labels(breaks))
    labels
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    list(self$get_labels_1d(1, breaks[[1]]),
         self$get_labels_1d(2, breaks[[2]]))
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

    trans1 <- scales::as.transform(self$transforms[[1]])
    trans2 <- scales::as.transform(self$transforms[[2]])

    limits_t <- list(sort(as.numeric(trans1$transform(limits[[1]]))), sort(as.numeric(trans2$transform(limits[[2]]))))

    breaks_t <- self$get_breaks_transformed(limits)

    x_bin <- cut(x1, breaks_t[[1]], include.lowest = TRUE, labels = FALSE)
    y_bin <- cut(x2, breaks_t[[2]], include.lowest = TRUE, labels = FALSE)

    in_limits <-
      is.finite(x1) & is.finite(x2) &
      x1 >= limits_t[[1]][1] & x1 <= limits_t[[1]][2] &
      x2 >= limits_t[[2]][1] & x2 <= limits_t[[2]][2]

    nx <- length(breaks_t[[1]]) - 1L
    id <- (y_bin - 1L) * nx + x_bin
    id[!in_limits] <- NA_integer_

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

  train = function(self, x)
    invisible(),
  train_df = function(self, df)
    invisible()
)

#' Bivariate colour scale constructor
#'
#' `bivariate_scale()` maps binned combinations of two variables to colour
#' dimensions and their combinations in perceptual colour space, supporting
#' the construction of bivariate choropleth maps.
#'
#' It can be automatically dispatched in `aes()` using `duo()` and works
#' with any ggplot2 geom.
#'
#' @inheritParams ggplot2::discrete_scale
#' @param breaks A list of two numeric vectors specifying bin boundaries for
#'   each variable. If `waiver()`, breaks are computed from the data according
#'   to `n_breaks` and `bin_method`.
#' @param labels A list of two character vectors or labelling functions
#'   used to label the bin boundaries for each variable. If `waiver()`,
#'   default numeric labels are used.
#' @param limits A list of two numeric vectors specifying the range of
#'   values to include for each variable.
#' @param transform A list of two transformations applied to the variables
#'   before binning. Each element can be a transformation name or a
#'   transformer object accepted by [scales::as.transform()].
#' @param colours A character vector of colours used as key points in the colour
#'   ramp that variables are mapped to. For details on how supplied colours are
#'   used to construct the resulting palette, see [bivar_palette()] and
#'   [bivar_fade_palette()].
#' @param palette_fun A palette function that, when called with `colours` and
#'   `n_breaks`, returns a character vector of colours for all binned
#'   combinations. If `NULL`, the default, [bivar_palette()] is used.
#' @param palette_params A list of additional arguments passed to `palette`. For
#'   details of arguments, see [bivar_palette()] and [bivar_fade_palette()].
#' @param n_breaks An integer or a length-two vector specifying the number of
#'   bins for each variable. The default is 4 for both variables, and unequal
#'   numbers of bins are supported.
#' @param bin_method A character string or a length-two vector specifying the
#'   method used to bin each variable: `"equal"` (the default) or `"quantile"`.
#' @param ... Other arguments passed to [ggplot2::discrete_scale()].
#' @param var1_name,var2_name Optional names for `v1` and `v2`. Used as axis
#'   titles in the legend. If `NULL`, the default, the names are taken from the
#'   mapping.
#' @returns A `ScaleBivariate` ggproto object.
#' @seealso [ggplot2::Scale] for the base ggproto class that all scale objects
#'   inherit from.
#' @export
bivariate_scale <- function(aesthetics,
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
                            colours = c("gold", "red4"),
                            palette_fun = NULL,
                            palette_params = list(),
                            n_breaks = c(4, 4),
                            bin_method = c("equal", "equal"),
                            var1_name = NULL,
                            var2_name = NULL,
                            super = ScaleBivariate) {
  normalize_pair <- function(x, name) {
    if (length(x) == 1)
      x <- rep(x, 2)
    if (length(x) != 2) {
      cli::cli_abort("{.arg {name}} must have length 1 or 2.")
    }
    x
  }

  normalize_pair_list <- function(x, name) {
    if (!is.list(x))
      x <- list(x)
    if (length(x) == 1)
      x <- rep(x, 2)
    if (length(x) != 2) {
      cli::cli_abort("{.arg {name}} must have length 1 or 2.")
    }
    x
  }

  resolve_palette <- function(palette_fun, colours, palette_params) {
    if (is.null(palette_fun)) {
      palette_fun <- bivar_palette
    }

    if (!is.function(palette_fun)) {
      cli::cli_abort("{.arg palette_fun} must be NULL or a function.")
    }

    function(n_breaks) {
      do.call(palette_fun, c(
        list(colours = colours, n_breaks = n_breaks),
        palette_params
      ))
    }
  }

  n_breaks <- normalize_pair(n_breaks, "n_breaks")
  bin_method <- normalize_pair(match.arg(bin_method, c("quantile", "equal"), several.ok = TRUE), "bin_method")
  transform <- normalize_pair_list(transform, "transform")
  breaks <- normalize_pair_list(breaks, "breaks")
  labels <- normalize_pair_list(labels, "labels")
  limits <- normalize_pair_list(limits, "limits")

  invisible(lapply(transform, scales::as.transform))

  sc <- discrete_scale(
    aesthetics = aesthetics,
    palette = function(n)
      seq_len(n),
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
  sc$colours <- colours
  sc$n_breaks <- n_breaks
  sc$bin_method <- bin_method
  sc$transforms <- transform
  sc$var1_name <- var1_name
  sc$var2_name <- var2_name
  sc$palette_fn <- resolve_palette(
    palette_fun = palette_fun,
    colours = colours,
    palette_params = palette_params
  )

  sc
}

#' @examples
#' # Basic bivariate map
#' ggplot(nc) +
#'   geom_sf(aes(fill = duo(value, sd)))
#'
#' # Use an alternative bivariate palette
#' ggplot(nc) +
#'   geom_sf(aes(fill = duo(value, sd))) +
#'   scale_fill_bivariate(
#'     palette_fun = bivar_fade_palette,
#'     colours = c("#F6E8C3", "orange", "red")
#'   )
#'
#' # Customize the number of bins
#' ggplot(nc) +
#'   geom_sf(aes(fill = duo(value, sd))) +
#'   scale_fill_bivariate(n_breaks = c(3, 4))
#' @rdname bivariate_scale
#' @export
scale_fill_bivariate <- function(...,
                                 name = waiver(),
                                 var1_name = NULL,
                                 var2_name = NULL,
                                 colours = c("gold", "red4"),
                                 palette_fun = NULL,
                                 palette_params = list(),
                                 n_breaks = c(4, 4),
                                 breaks = list(waiver(), waiver()),
                                 labels = list(waiver(), waiver()),
                                 limits = list(NULL, NULL),
                                 transform = list("identity", "identity"),
                                 bin_method = c("equal", "equal"),
                                 na.value = NA,
                                 aesthetics = "fill",
                                 guide = guide_bivariate()) {
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
    colours = colours,
    palette_fun = palette_fun,
    palette_params = palette_params,
    n_breaks = n_breaks,
    bin_method = bin_method,
    var1_name = var1_name,
    var2_name = var2_name
  )
}

#' @rdname bivariate_scale
#' @export
scale_color_bivariate <- function(...,
                                  name = waiver(),
                                  var1_name = NULL,
                                  var2_name = NULL,
                                  colours = c("gold", "red4"),
                                  palette_fun = NULL,
                                  palette_params = list(),
                                  n_breaks = c(4, 4),
                                  breaks = list(waiver(), waiver()),
                                  labels = list(waiver(), waiver()),
                                  limits = list(NULL, NULL),
                                  transform = list("identity", "identity"),
                                  bin_method = c("equal", "equal"),
                                  na.value = NA,
                                  aesthetics = "colour",
                                  guide = guide_bivariate()) {
  scale_fill_bivariate(
    name = name,
    var1_name = var1_name,
    var2_name = var2_name,
    colours = colours,
    palette_fun = palette_fun,
    palette_params = palette_params,
    n_breaks = n_breaks,
    breaks = breaks,
    labels = labels,
    limits = limits,
    transform = transform,
    bin_method = bin_method,
    na.value = na.value,
    aesthetics = aesthetics,
    guide = guide,
    ...
  )
}

#' @rdname bivariate_scale
#' @export
scale_colour_bivariate <- scale_color_bivariate

#' @export
scale_type.bivariate <- function(x) "bivariate"
