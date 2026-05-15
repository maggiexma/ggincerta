scale_angle_continuous <- function(...,
                                   name = waiver(),
                                   order = 2) {
  continuous_scale(
    aesthetics = "angle",
    scale_name = "angle",
    palette = scales::identity_pal(),
    name = name,
    guide = guide_glyph(order = order),
    ...
  )
}
