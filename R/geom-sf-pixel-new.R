StatPixelPj <- ggproto(
  "StatPixel",
  StatSf,
  required_aes = c("fill"),

  compute_panel = function(data,
                           scales,
                           coord,
                           n,
                           distribution,
                           seed,
                           pixel_crs = 3857) {
    label <- {
      vars <- attr(data$fill, "vars", exact = TRUE)
      if (!is.null(vars) && length(vars) >= 1L) {
        rlang::as_label(vars[[1L]])
      } else {
        NULL
      }
    }

    sf_data <- sf::st_as_sf(data)
    sf_data$ID <- seq_len(nrow(sf_data))

    sf_data$v1 <- purrr::map_dbl(sf_data$fill, "v1")
    sf_data$v2 <- purrr::map_dbl(sf_data$fill, "v2")

    distribution <- rlang::arg_match(distribution, c("uniform", "normal"))

    crs_orig <- sf::st_crs(sf_data)

    if (is.na(crs_orig)) {
      rlang::abort("Input data must have a valid CRS.")
    }

    is_longlat <- sf::st_is_longlat(sf_data)
    unit_name <- crs_orig$units_gdal
    is_metric <- !is.null(unit_name) &&
      tolower(unit_name) %in% c("metre", "meter")

    if (!is_longlat && is_metric) {
      sf_proj <- sf_data
    } else {
      pixel_crs <- sf::st_crs(pixel_crs)
      if (is.na(pixel_crs)) {
        rlang::abort("`pixel_crs` must be a valid CRS.")
      }
      sf_proj <- sf::st_transform(sf_data, pixel_crs)
    }

    grid_sf <- sf::st_sf(
      geometry = sf::st_make_grid(sf_proj, n = n),
      crs = sf::st_crs(sf_proj)
    )

    pix_sf <- suppressWarnings(sf::st_intersection(sf_proj[, c("ID", "v1", "v2")], grid_sf))

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

    pix_sf <- sf::st_transform(pix_sf, crs_orig)

    if (!is.null(label)) {
      attr(pix_sf$fill, "label") <- label
    }

    pix_sf
  }
)

geom_sf_pixe_new <- function(mapping = NULL,
                          data = NULL,
                          n = 60,
                          distribution = "uniform",
                          seed = NULL,
                          pixel_crs = 3857,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  list(
    layer_sf(
      data = data,
      mapping = mapping,
      stat = StatPixelPj,
      geom = "sf",
      position = "identity",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        colour = NA,
        n = n,
        distribution = distribution,
        seed = seed,
        pixel_crs = pixel_crs,
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
