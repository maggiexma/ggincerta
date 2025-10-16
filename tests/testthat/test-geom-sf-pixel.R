test_that('geom_sf_pixel works', {
  ggplot(nc) +
    geom_sf_pixel(mapping = aes(v1 = value, v2 = sd, id = NAME))

  nc.q <- with(nc,
               data.frame(NAME = nc$NAME,
                 p0.05 = qnorm(0.05, mean = value, sd = 1),
                 p0.25 = qnorm(0.25, mean = value, sd = 1),
                 p0.5 = qnorm(0.5, mean = value, sd = 1),
                 p0.75 = qnorm(0.75, mean = value, sd = 1),
                 p0.95 = qnorm(0.95, mean = value, sd = 1))
               )

  ggplot(nc) +
    geom_sf_pixel(mapping = aes(v1 = value, v2 = sd, id = NAME), distribution = 'discrete',
                  pixel_size = 40, q = nc.q)

  vdiffr::expect_doppelganger("pixel map", p)
})
