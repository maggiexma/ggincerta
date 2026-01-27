#' @rdname ggsfpi
#' @export
StatPixel <- ggproto(
  "StatPixel",
  StatSf,
  required_aes = c("fill"),

  compute_panel = function(data, scales, coord, n, distribution, seed) {
    label <- {
      vars <- attr(data$fill, "vars", exact = TRUE)
      if (!is.null(vars) &&
          length(vars) >= 1L)
        rlang::as_label(vars[[1L]])
      else
        NULL
    }

    data <- StatSf$compute_panel(data, scales, coord)

    sf_data <- sf::st_as_sf(data)
    sf_data$ID <- seq_len(nrow(sf_data))

    sf_data$v1 <- purrr::map_dbl(sf_data$fill, "v1")
    sf_data$v2 <- purrr::map_dbl(sf_data$fill, "v2")

    grid_sf <- sf::st_sf(
      geometry = sf::st_make_grid(sf_data, n = n),
      crs = sf::st_crs(sf_data)
    )

    pix_sf <- suppressWarnings(sf::st_intersection(sf_data[, c("ID", "v1", "v2")], grid_sf))
    pix_sf <- sf::st_make_valid(pix_sf)

    types <- unique(as.character(sf::st_geometry_type(pix_sf, by_geometry = TRUE)))
    if ("GEOMETRYCOLLECTION" %in% types) {
      pix_sf <- sf::st_collection_extract(pix_sf, "POLYGON", warn = FALSE)
      types <- unique(as.character(sf::st_geometry_type(pix_sf, by_geometry = TRUE)))
    }
    if ("MULTIPOLYGON" %in% types) {
      pix_sf <- sf::st_cast(pix_sf, "POLYGON", warn = FALSE)
    }

    if (nrow(pix_sf) == 0L)
      return(pix_sf)

    is_empty <- sf::st_is_empty(pix_sf)
    if (any(is_empty))
      pix_sf <- pix_sf[!is_empty, , drop = FALSE]
    if (nrow(pix_sf) == 0L)
      return(pix_sf)

    has_area <- as.numeric(sf::st_area(pix_sf)) > 0
    if (any(!has_area))
      pix_sf <- pix_sf[has_area, , drop = FALSE]
    if (nrow(pix_sf) == 0L)
      return(pix_sf)

    distribution <- rlang::arg_match(distribution, c("uniform", "normal"))

    sample_fill <- function(pix_sf, distribution) {
      dplyr::group_by(pix_sf, ID) |>
        dplyr::mutate(fill = {
          m <- dplyr::first(v1)
          s <- dplyr::first(v2)

          if (is.na(m) || is.na(s)) {
            rep(NA_real_, dplyr::n())
          } else if (distribution == "uniform") {
            vec <- seq(m - s, m + s, length.out = 5)
            sample(vec, dplyr::n(), replace = TRUE)
          } else {
            stats::rnorm(dplyr::n(), mean = m, sd = s)
          }
        }) |>
        dplyr::ungroup()
    }

    if (is.null(seed)) {
      pix_sf <- sample_fill(pix_sf, distribution)
    } else {
      if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
        rlang::abort("`seed` must be a finite numeric scalar.")
      }
      pix_sf <- withr::with_seed(as.integer(seed), sample_fill(pix_sf, distribution))
    }

    if (!is.null(label))
      attr(pix_sf$fill, "label") <- label

    pix_sf
  }
)


#' Generate pixel maps on sf objects
#'
#' `geom_sf_pixel()` adds a pixel map layer based on simple feature (sf) objects
#' to a ggplot. In a pixel map, each region is divided into small pixels, with
#' colours mapped from values sampled from distribution specified.
#'
#' @section Pixel map layer contents:
#' The layer returned by `geom_sf_pixel()` internally includes a scale object
#' created by `scale_fill_distiller()`.
#' Therefore, modifying the scale will trigger a message indicating that
#' the scale for `fill` is being replaced.
#'
#' @inheritParams ggplot2::geom_sf
#' @inheritParams sf::st_make_grid
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   `v1` and `v2` are required, which are the variables used as the parameters
#'   in the sampling distribution.
#' @param distribution Sampling distribution: `"uniform"`(the default) or, `"normal"`.
#'   * `"uniform"` treats `v1` as the centre and uniformly samples within the range
#'     `(v1 - v2, v1 + v2)`.
#'   * `"normal"` treats `v1` as the mean and `v2` as the standard deviation.
#' @param seed Random seed used to ensure reproducibility of the sampling process.
#'
#' @returns A list of ggplot2 layer objects.
#'
#' @examples
#' # Basic pixel map
#' p <- ggplot(nc) + geom_sf_pixel(mapping = aes(v1 = value, v2 = sd), n = 20)
#'
#' # Replacing the internal fill scale triggers a message
#' # ("Scale for fill is already present. Adding another scale for fill...")
#' p + scale_fill_distiller(palette = "Blues")
#'
#' @rdname ggsfpi
#' @export
geom_sf_pixel <- function(mapping = NULL,
                          data = NULL,
                          n = 40,
                          distribution = "uniform",
                          seed = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  list(
    layer_sf(
      data = data,
      mapping = mapping,
      stat = StatPixel,
      geom = "sf",
      position = "identity",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        n = n,
        distribution = distribution,
        seed = seed,
        ...
      )
    ),
    geom_sf(
      fill = NA,
      color = "black",
      linewidth = 0.7
    ),
    coord_sf()
  )
}

ggplot(nc) +
  geom_sf_pixel(aes(fill = duo_pixel(value, sd)), n = 30)
