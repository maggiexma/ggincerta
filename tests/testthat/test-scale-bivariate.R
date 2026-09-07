expected_bivariate_fill <- function(data,
                                    x,
                                    y,
                                    n_breaks = c(4, 4),
                                    colours = c("#EDF8B1", "#2C7FB8"),
                                    nice.breaks = TRUE,
                                    quantile = FALSE,
                                    limits = list(NULL, NULL),
                                    transform = list("identity", "identity")) {
  n_breaks <- rep(n_breaks, length.out = 2)
  nice.breaks <- rep(nice.breaks, length.out = 2)
  quantile <- rep(quantile, length.out = 2)

  if (!is.list(limits)) {
    limits <- list(limits)
  }

  if (length(limits) == 1) {
    limits <- rep(limits, 2)
  }

  if (!is.list(transform)) {
    transform <- list(transform)
  }

  if (length(transform) == 1) {
    transform <- rep(transform, 2)
  }

  get_breaks <- function(z, n, nice, quantile, limits, transform) {
    trans <- scales::as.transform(transform)

    explicit_limits <- !is.null(limits)

    if (is.null(limits)) {
      limits <- range(z, na.rm = TRUE)
    }

    limits <- sort(limits)
    limits_t <- sort(trans$transform(limits))

    z_t <- trans$transform(z)

    if (quantile) {
      keep <- is.finite(z) &
        is.finite(z_t) &
        z >= limits[1] &
        z <= limits[2]

      z_t <- z_t[keep]

      breaks_t <- quantile(
        z_t,
        probs = seq(0, 1, length.out = n + 1),
        na.rm = TRUE,
        names = FALSE
      )

      if (length(unique(breaks_t)) < n + 1) {
        breaks_t <- seq(limits_t[1], limits_t[2], length.out = n + 1)
      } else {
        breaks_t[c(1, length(breaks_t))] <- limits_t
      }

      return(breaks_t)
    }

    if (!nice) {
      return(seq(limits_t[1], limits_t[2], length.out = n + 1))
    }

    breaks_t <- pretty(limits_t, n = n)

    if (explicit_limits) {
      internal_t <- breaks_t[breaks_t > limits_t[1] &
                               breaks_t < limits_t[2]]

      breaks_t <- c(limits_t[1], internal_t, limits_t[2])
    }

    breaks_t
  }

  trans_x <- scales::as.transform(transform[[1]])
  trans_y <- scales::as.transform(transform[[2]])

  x_t <- trans_x$transform(data[[x]])
  y_t <- trans_y$transform(data[[y]])

  bx <- get_breaks(
    data[[x]],
    n = n_breaks[1],
    nice = nice.breaks[1],
    quantile = quantile[1],
    limits = limits[[1]],
    transform = transform[[1]]
  )

  by <- get_breaks(
    data[[y]],
    n = n_breaks[2],
    nice = nice.breaks[2],
    quantile = quantile[2],
    limits = limits[[2]],
    transform = transform[[2]]
  )

  bin1 <- cut(x_t,
              breaks = bx,
              include.lowest = TRUE,
              labels = FALSE)

  bin2 <- cut(y_t,
              breaks = by,
              include.lowest = TRUE,
              labels = FALSE)

  actual_n_breaks <- c(length(bx) - 1L, length(by) - 1L)

  combo <- (bin2 - 1L) * actual_n_breaks[1] + bin1

  pal <- bivar_palette(colours = colours, n_breaks = actual_n_breaks)

  unname(pal[combo])
}


test_that("bivariate scale maps default pretty bin combinations to colours",
          {
            p <- ggplot(nc) +
              geom_sf(aes(fill = duo(value, sd)))

            fills_mapped <- ggplot_build(p)$data[[1]]$fill

            fills_expected <- expected_bivariate_fill(nc, x = "value", y = "sd")

            expect_equal(fills_mapped, fills_expected)
          })


test_that("default pretty breaks treat n_breaks as a suggestion", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(n_breaks = 4)

  gb <- ggplot_build(p)
  scale <- gb$plot$scales$get_scales("fill")

  breaks <- scale$get_breaks()
  breaks_t <- scale$get_breaks_transformed()
  n_actual <- scale$get_n_breaks()

  expect_equal(n_actual, c(length(breaks[[1]]) - 1L, length(breaks[[2]]) - 1L))

  expect_equal(breaks_t[[1]], pretty(range(nc$value, na.rm = TRUE), n = 4))

  expect_equal(breaks_t[[2]], pretty(range(nc$sd, na.rm = TRUE), n = 4))
})


test_that("bivariate scale maps quantile bin combinations to colours", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(quantile = TRUE)

  fills_mapped <- ggplot_build(p)$data[[1]]$fill

  fills_expected <- expected_bivariate_fill(nc,
                                            x = "value",
                                            y = "sd",
                                            quantile = TRUE)

  expect_equal(fills_mapped, fills_expected)
})


test_that("bivariate scale works with unequal desired numbers of breaks", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(n_breaks = c(3, 4))

  fills_mapped <- ggplot_build(p)$data[[1]]$fill

  fills_expected <- expected_bivariate_fill(nc,
                                            x = "value",
                                            y = "sd",
                                            n_breaks = c(3, 4))

  expect_equal(fills_mapped, fills_expected)
})


test_that("bivariate scale uses exact equal-width bins when nice.breaks is FALSE",
          {
            p <- ggplot(nc) +
              geom_sf(aes(fill = duo(value, sd))) +
              scale_fill_bivariate(n_breaks = 4, nice.breaks = FALSE)

            gb <- ggplot_build(p)

            fills_mapped <- gb$data[[1]]$fill

            fills_expected <- expected_bivariate_fill(
              nc,
              x = "value",
              y = "sd",
              n_breaks = 4,
              nice.breaks = FALSE
            )

            scale <- gb$plot$scales$get_scales("fill")

            expect_equal(fills_mapped, fills_expected)

            expect_equal(scale$get_n_breaks(), c(4, 4))
          })


test_that("bivariate scale supports unequal exact bin counts", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(n_breaks = c(3, 5), nice.breaks = FALSE)

  gb <- ggplot_build(p)
  scale <- gb$plot$scales$get_scales("fill")

  expect_equal(scale$get_n_breaks(), c(3, 5))
})


test_that("explicit limits are preserved with pretty breaks", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(limits = list(c(-3, 3), c(0, 4)), n_breaks = 4)

  gb <- ggplot_build(p)
  scale <- gb$plot$scales$get_scales("fill")

  breaks <- scale$get_breaks()

  expect_equal(range(breaks[[1]]), c(-3, 3))

  expect_equal(range(breaks[[2]]), c(0, 4))
})


test_that("explicit limits use pretty internal breaks", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(limits = list(c(-3, 3), c(0, 4)), n_breaks = 4)

  gb <- ggplot_build(p)
  scale <- gb$plot$scales$get_scales("fill")

  expected_x <- pretty(c(-3, 3), n = 4)

  expected_x <- expected_x[expected_x > -3 &
                             expected_x < 3]

  expected_x <- c(-3, expected_x, 3)

  expected_y <- pretty(c(0, 4), n = 4)

  expected_y <- expected_y[expected_y > 0 &
                             expected_y < 4]

  expected_y <- c(0, expected_y, 4)

  expect_equal(scale$get_breaks_transformed()[[1]], expected_x)

  expect_equal(scale$get_breaks_transformed()[[2]], expected_y)
})


test_that("bivariate scale handles missing values in ggplot", {
  nc_na <- nc

  nc_na$value[1:3] <- NA
  nc_na$sd[c(2, 4)] <- NA

  p <- ggplot(nc_na) +
    geom_sf(aes(fill = duo(value, sd)))

  fills_mapped <- ggplot_build(p)$data[[1]]$fill

  expect_true(all(is.na(fills_mapped[c(1, 2, 3, 4)])))

  expect_false(any(is.na(fills_mapped[-c(1, 2, 3, 4)])))
})


test_that("bivariate scale works with custom palette", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(
      palette_fun = bivar_fade_palette,
      colours = c("red", "yellow", "green", "blue", "black")
    )

  expect_s3_class(ggplot_build(p), "ggplot_built")
})


test_that("manual bivariate scale uses supplied values", {
  vals <- c(
    "#F7F4F9",
    "#D4B9DA",
    "#C994C7",
    "#980043",
    "#E0ECF4",
    "#BFD3E6",
    "#9EBCDA",
    "#8856A7",
    "#D0D1E6",
    "#A6BDDB",
    "#74A9CF",
    "#2B8CBE",
    "#B8E186",
    "#7FBC41",
    "#4D9221",
    "#276419"
  )

  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate_manual(values = vals)

  gb <- ggplot_build(p)

  fills_mapped <- gb$data[[1]]$fill
  scale <- gb$plot$scales$get_scales("fill")

  expect_true(all(na.omit(fills_mapped) %in% vals))

  expect_equal(scale$get_n_breaks(), c(4, 4))
})


test_that("manual bivariate scale requires enough values", {
  expect_snapshot_error(ggplot_build(
    ggplot(nc) +
      geom_sf(aes(fill = duo(value, sd))) +
      scale_fill_bivariate_manual(values = c("red", "blue"))
  ))
})


test_that("manual bivariate scale supports unequal numbers of bins", {
  vals <- grDevices::hcl.colors(12, "Viridis")

  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate_manual(values = vals, n_breaks = c(3, 4))

  gb <- ggplot_build(p)

  scale <- gb$plot$scales$get_scales("fill")
  fills_mapped <- gb$data[[1]]$fill

  expect_equal(scale$get_n_breaks(), c(3, 4))

  expect_true(all(na.omit(fills_mapped) %in% vals))
})


test_that("manual bivariate scale checks colours against bin counts", {
  expect_error(
    ggplot_build(
      ggplot(nc) +
        geom_sf(aes(fill = duo(value, sd))) +
        scale_fill_bivariate_manual(values = rep("red", 11), n_breaks = c(3, 4))
    ),
    "Manual bivariate scale needs 12 colours, but only 11 provided."
  )
})


test_that("bivariate scale works with labs and theme", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(n_breaks = 4) +
    labs(title = "bivariate map on nc") +
    theme(legend.position = "left")

  gb <- ggplot_build(p)

  expect_equal(gb$plot$labels$title, "bivariate map on nc")

  vdiffr::expect_doppelganger("bivariate map with left guide", p)
})


test_that("bivariate scale works automatically with geom_sf", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd)))

  vdiffr::expect_doppelganger("bivariate map", p)
})


test_that("bivariate scale respects custom breaks, limits, labels, and transform",
          {
            p <- ggplot(nc) +
              geom_sf(aes(fill = duo(value, sd))) +
              scale_fill_bivariate(
                breaks = list(c(0, 2, 4, 6, 8), c(1, 2, 4, 8, 16)),
                limits = list(c(0, 8), c(1, 16)),
                labels = list(
                  c("very low", "low", "high", "very high"),
                  c("small", "medium", "large", "very large")
                ),
                transform = list("identity", "log2"),
                n_breaks = c(4, 4)
              )

            gb <- ggplot_build(p)
            fills_mapped <- gb$data[[1]]$fill

            expect_equal(length(fills_mapped), nrow(nc))

            expect_true(any(is.na(fills_mapped)))

            expect_true(any(!is.na(fills_mapped)))

            guide_info <-
              gb$plot$scales$get_scales("fill")$get_guide_info()

            expect_equal(guide_info$x_breaks, c(0, 2, 4, 6, 8))

            expect_equal(guide_info$y_breaks, c(1, 2, 4, 8, 16))

            expect_equal(guide_info$x_labels, c("very low", "low", "high", "very high"))

            expect_equal(guide_info$y_labels,
                         c("small", "medium", "large", "very large"))
          })


test_that("bivariate scale supports different binning methods for each variable",
          {
            p <- ggplot(nc) +
              geom_sf(aes(fill = duo(value, sd))) +
              scale_fill_bivariate(nice.breaks = c(TRUE, FALSE),
                                   quantile = c(FALSE, TRUE))

            fills_mapped <- ggplot_build(p)$data[[1]]$fill

            fills_expected <- expected_bivariate_fill(
              nc,
              x = "value",
              y = "sd",
              nice.breaks = c(TRUE, FALSE),
              quantile = c(FALSE, TRUE)
            )

            expect_equal(fills_mapped, fills_expected)
          })


test_that("bivariate scale transforms custom breaks and limits before binning",
          {
            x <- duo(c(1, 2, 3, 4), c(1, 2, 4, 16))

            scale <- scale_fill_bivariate(
              transform = list("identity", "log2"),
              breaks = list(c(0, 2, 4), c(1, 2, 4, 8, 16)),
              limits = list(c(0, 4), c(1, 16)),
              na.value = "grey80"
            )

            scale$train(x)

            mapped <- scale$map(scale$transform(x))

            expect_false(any(is.na(mapped)))

            expect_false(any(mapped == "grey80"))
          })


test_that("pretty breaks are computed on the transformed scale", {
  df <- data.frame(x = c(1, 10, 100, 1000), y = c(1, 10, 100, 1000))

  p <- ggplot(df) +
    geom_point(aes(
      x = x,
      y = y,
      colour = duo(x, y)
    )) +
    scale_colour_bivariate(transform = "log10", n_breaks = 4)

  gb <- ggplot_build(p)
  scale <- gb$plot$scales$get_scales("colour")

  breaks_t <- scale$get_breaks_transformed()

  expected <- pretty(c(0, 3), n = 4)

  expect_equal(breaks_t[[1]], expected)

  expect_equal(breaks_t[[2]], expected)
})


test_that("exact equal-width bins are computed on the transformed scale", {
  df <- data.frame(x = c(1, 10, 100, 1000), y = 1:4)

  p <- ggplot(df) +
    geom_point(aes(
      x = x,
      y = y,
      colour = duo(x, y)
    )) +
    scale_colour_bivariate(
      transform = list("log10", "identity"),
      limits = list(c(1, 1000), c(1, 4)),
      n_breaks = c(3, 3),
      nice.breaks = FALSE
    )

  gb <- ggplot_build(p)
  scale <- gb$plot$scales$get_scales("colour")

  expect_equal(scale$get_breaks_transformed()[[1]], c(0, 1, 2, 3))

  expect_equal(scale$get_breaks()[[1]], c(1, 10, 100, 1000))

  expect_equal(scale$get_n_breaks(), c(3, 3))
})


test_that("bivariate scale applies transformed limits when mapping", {
  x <- duo(c(1, 2, 3), c(1, 4, 32))

  scale <- scale_fill_bivariate(
    transform = list("identity", "log2"),
    breaks = list(c(0, 1, 2, 3), c(1, 2, 4, 8, 16, 32)),
    limits = list(c(0, 3), c(1, 16)),
    na.value = "grey80"
  )

  scale$train(x)

  mapped <- scale$map(scale$transform(x))

  expect_equal(mapped[3], "grey80")

  expect_false(mapped[1] == "grey80")

  expect_false(mapped[2] == "grey80")
})


test_that("bivariate scale maps missing values to na.value", {
  x <- duo(c(1, 2, NA, 4), c(1, NA, 3, 4))

  scale <- scale_fill_bivariate(na.value = "grey80")

  scale$train(x)

  mapped <- scale$map(scale$transform(x))

  expect_equal(length(mapped), 4)

  expect_equal(mapped[2], "grey80")

  expect_equal(mapped[3], "grey80")

  expect_false(mapped[1] == "grey80")

  expect_false(mapped[4] == "grey80")
})
