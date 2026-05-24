scale_angle_continuous <- function(...,
                                   name = waiver(),
                                   order = 99) {
  continuous_scale(
    aesthetics = "angle",
    palette = scales::identity_pal(),
    name = name,
    guide = guide_glyph(order = order),
    ...
  )
}
