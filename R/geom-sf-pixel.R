#' @rdname geom_sf_pixel
#' @export
StatPixel <- ggproto(
  "StatPixel",
  StatSf,
  required_aes = "fill",

  compute_panel = function(data,
                           scales,
                           coord,
                           n,
                           distribution,
                           seed,
                           pixel_shape = "hex",
                           flat_topped = FALSE) {
    pixel_shape <- rlang::arg_match(pixel_shape, values = c("rect", "square", "hex"))

    distribution <- rlang::arg_match(distribution, values = c("uniform", "normal"))

    label <- {
      vars <- attr(data$fill, "vars", exact = TRUE)
      if (!is.null(vars) && length(vars) >= 1L) {
        rlang::as_label(vars[[1L]])
      } else {
        NULL
      }
    }

    sf_data <- sf::st_as_sf(data)
    crs <- sf::st_crs(sf_data)

    if (is.na(crs)) {
      rlang::abort("Input data must have a valid CRS.")
    }

    sf_data$ID <- seq_len(nrow(sf_data))
    sf_data$v1 <- purrr::map_dbl(sf_data$fill, "v1")
    sf_data$v2 <- purrr::map_dbl(sf_data$fill, "v2")

    make_pixel_grid <- function(x, shape, n, flat_topped = FALSE) {
      if (length(n) == 1L) {
        n <- rep(n, 2)
      }

      bb <- sf::st_bbox(x)
      xrange <- unname(bb["xmax"] - bb["xmin"])
      yrange <- unname(bb["ymax"] - bb["ymin"])

      if (shape == "rect") {
        grid <- sf::st_make_grid(x, n = n)
      } else {
        cellsize <- min(xrange / n[1], yrange / n[2])
        grid <- sf::st_make_grid(
          x,
          cellsize = cellsize,
          square = (shape == "square"),
          flat_topped = flat_topped
        )
      }

      sf::st_sf(geometry = grid, crs = sf::st_crs(x))
    }

    grid_sf <- make_pixel_grid(
      x = sf_data,
      shape = pixel_shape,
      n = n,
      flat_topped = flat_topped
    )

    if (isTRUE(sf::st_is_longlat(sf_data)) &&
        isTRUE(sf::sf_use_s2())) {
      message(
        paste(
          "geom_sf_pixel_new(): input data has a geographic CRS and sf is using s2;",
          "pixelation may be slow.",
          "Consider transforming to a projected planar CRS first."
        )
      )
      flush.console()
    }

    pix_sf <- suppressWarnings(sf::st_intersection(sf_data[, c("ID", "v1", "v2")], grid_sf))
    pix_sf <- suppressWarnings(sf::st_make_valid(pix_sf))
    pix_sf <- suppressWarnings(sf::st_collection_extract(pix_sf, "POLYGON", warn = FALSE))

    is_empty <- sf::st_is_empty(pix_sf)
    if (any(is_empty)) {
      pix_sf <- pix_sf[!is_empty, , drop = FALSE]
    }
    if (nrow(pix_sf) == 0L) {
      return(pix_sf)
    }

    pix_sf <- suppressWarnings(sf::st_cast(pix_sf, "POLYGON", warn = FALSE))

    has_area <- as.numeric(sf::st_area(pix_sf)) > 0
    if (any(!has_area)) {
      pix_sf <- pix_sf[has_area, , drop = FALSE]
    }
    if (nrow(pix_sf) == 0L) {
      return(pix_sf)
    }

    sample_fill <- function(x, distribution) {
      dplyr::group_by(x, ID) |>
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

    if (!is.null(label)) {
      attr(pix_sf$fill, "label") <- label
    }

    pix_sf
  }
)

#' Pixel map
#'
#' `geom_sf_pixel()` generates a pixel map layer on areal sf data. Each region
#' is tessellated into small pixels, with pixel colours mapped from values
#' sampled from a specified distribution.
#'
#' Mappings in `geom_sf_pixel()` is also supplied with [duo_pixel()] inside
#' `aes()`, which automatically dispatches an scale.
#'
#' Since [sf::st_intersection()] is used internally, operating directly on
#' geographic (s2) sf objects can be slow, especially when a large number of
#' pixels are generated. Projecting data to a planar coordinate system in
#' advance is recommended.
#'
#' @inheritParams ggplot2::geom_sf
#' @inheritParams sf::st_make_grid
#' @param distribution Distribution used to sample pixel values within each
#'   region. Currently supports `"uniform"` (the default) and `"normal"`.
#' @param seed Integer seed used for reproducible sampling.
#' @param pixel_shape Shape of the generated pixels. One of `"hex"` (the default), `"square"`,
#'   or `"rect"`. `"rect"` is when dividing the x and y ranges into the same
#'   number of intervals, so cells may be rectangular.
#' @returns A list of ggplot2 layer objects.
#' @examples
#' # Transform sf data into a planar crs for faster geometric intersection
#' nc_flat <- sf::st_transform(nc, sf::st_crs(3857))
#'
#' # Basic pixel map
#' ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
#'   geom_sf_pixel(n = 40)
#'
#' # Control pixel shape and resolution
#' ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
#'   geom_sf_pixel(n = 30, pixel_shape = "square")
#' @export
geom_sf_pixel <- function(mapping = NULL,
                          data = NULL,
                          n = 60,
                          distribution = "uniform",
                          seed = NULL,
                          pixel_shape = "hex",
                          flat_topped = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  pixel_shape <- rlang::arg_match(pixel_shape, values = c("rect", "square", "hex"))

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
        colour = NA,
        n = n,
        distribution = distribution,
        seed = seed,
        pixel_shape = pixel_shape,
        flat_topped = flat_topped,
        ...
      )
    ),
    geom_sf(
      fill = NA,
      color = "black",
      linewidth = 0.2
    )
  )
}
