expected_bivariate_fill <- function(data,
                                    x,
                                    y,
                                    n_breaks = c(4, 4),
                                    colours = c("gold", "red4"),
                                    bin_method = c("equal", "equal")) {
  bin_method <- rep(bin_method, length.out = 2)

  get_breaks <- function(z, n, method) {
    if (method == "quantile") {
      quantile(z, seq(0, 1, length.out = n + 1), na.rm = TRUE)
    } else {
      seq(min(z, na.rm = TRUE), max(z, na.rm = TRUE), length.out = n + 1)
    }
  }

  bx <- get_breaks(data[[x]], n_breaks[1], bin_method[1])
  by <- get_breaks(data[[y]], n_breaks[2], bin_method[2])

  bin1 <- cut(
    data[[x]],
    breaks = bx,
    include.lowest = TRUE,
    labels = FALSE
  )
  bin2 <- cut(
    data[[y]],
    breaks = by,
    include.lowest = TRUE,
    labels = FALSE
  )

  combo <- (bin2 - 1L) * n_breaks[1] + bin1
  pal <- bivar_palette(colours = colours, n_breaks = n_breaks)

  unname(pal[combo])
}

test_that("bivariate scale maps default equal bin combinations to colours", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd)))

  fills_mapped <- ggplot_build(p)$data[[1]]$fill

  fills_expected <- expected_bivariate_fill(nc, x = "value", y = "sd")

  expect_equal(fills_mapped, fills_expected)
})

test_that("bivariate scale maps quantile bin combinations to colours", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(bin_method = "quantile")

  fills_mapped <- ggplot_build(p)$data[[1]]$fill

  fills_expected <- expected_bivariate_fill(
    nc,
    x = "value",
    y = "sd",
    bin_method = c("quantile", "quantile")
  )

  expect_equal(fills_mapped, fills_expected)
})

test_that("bivariate scale works with unequal numbers of breaks", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(n_breaks = c(3, 4))

  fills_mapped <- ggplot_build(p)$data[[1]]$fill

  fills_expected <- expected_bivariate_fill(
    nc,
    x = "value",
    y = "sd",
    n_breaks = c(3, 4),
    colours = c("gold", "red4")
  )

  expect_equal(fills_mapped, fills_expected)
})

test_that("bivariate scale handles missing values", {
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
    scale_fill_bivariate(palette_fun = bivar_fade_palette,
                         colours = c("red", "yellow", "green", "blue", "black"))

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

  fills_mapped <- ggplot_build(p)$data[[1]]$fill

  expect_true(all(na.omit(fills_mapped) %in% vals))
})

test_that("manual bivariate scale requires enough values", {
  expect_snapshot_error(ggplot_build(
    ggplot(nc) +
      geom_sf(aes(fill = duo(value, sd))) +
      scale_fill_bivariate_manual(values = c("red", "blue"))
  ))
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
                n_breaks = c(4, 4),
                bin_method = c("equal", "equal")
              )

            gb <- ggplot_build(p)
            fills_mapped <- gb$data[[1]]$fill

            expect_equal(length(fills_mapped), nrow(nc))
            expect_true(any(is.na(fills_mapped)))
            expect_true(any(!is.na(fills_mapped)))

            guide_info <- gb$plot$scales$get_scales("fill")$get_guide_info()

            expect_equal(guide_info$x_breaks, c(0, 2, 4, 6, 8))
            expect_equal(guide_info$y_breaks, c(1, 2, 4, 8, 16))

            expect_equal(guide_info$x_labels, c("very low", "low", "high", "very high"))

            expect_equal(guide_info$y_labels,
                         c("small", "medium", "large", "very large"))
          })

test_that("bivariate scale transforms breaks and limits before binning", {
  x <- duo(c(1, 2, 3, 4), c(1, 2, 4, 16))

  scale <- scale_fill_bivariate(
    transform = list("identity", "log2"),
    breaks = list(c(0, 2, 4), c(1, 2, 4, 8, 16)),
    limits = list(c(0, 4), c(1, 16)),
    bin_method = c("equal", "equal"),
    na.value = "grey80"
  )

  scale$train(x)
  mapped <- scale$map(scale$transform(x))

  expect_false(any(is.na(mapped)))
  expect_false(any(mapped == "grey80"))
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
  scale$get_limits()
  mapped <- scale$map(scale$transform(x))

  expect_equal(mapped[3], "grey80")
  expect_false(mapped[1] == "grey80")
  expect_false(mapped[2] == "grey80")
})

test_that("bivariate scale handles missing values", {
  x <- duo(c(1, 2, NA, 4), c(1, NA, 3, 4))

  scale <- scale_fill_bivariate(na.value = "grey80")
  scale$train(x)

  mapped <- scale$map(x)

  expect_equal(length(mapped), 4)
  expect_equal(mapped[3], "grey80")
  expect_equal(mapped[2], "grey80")
  expect_false(is.na(mapped[1]))
  expect_false(is.na(mapped[4]))
})

df <- data.frame(x = c(1, 10, 100, 1000), y = 1:4)
