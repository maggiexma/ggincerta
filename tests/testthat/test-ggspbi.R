test_that("StatSPBivariate works", {
  data(UB, package = "Vizumap") # this returns a data frame, UB_tss, and a shapefile, UB_shp
  UB_dat <- read.uv(data = UB_tss, estimate = "TSS", error = "TSS_error")

  # use vdiffr to build tests
  # wrap image using expect_doppelganger()

  ggplot() +
    stat_sp_bivariate(
      data = UB_dat,
      sp_object = UB_shp,
      id_col = 'scID',
      estimate = 'TSS',
      error = 'TSS_error',
      terciles = TRUE
    ) +
    geom_sf(
      data = UB_sf,
      fill = NA,
      color = "black",
      size = 0.3,
      inherit.aes = FALSE
    ) +
    scale_fill_bivar(colrange = list(colour = c('gold','red4'), difC = c(4,4))) +
    coord_sf() +
    theme_void()


})
