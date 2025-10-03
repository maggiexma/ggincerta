#' @export
GuideBivariate <- ggproto(
  "GuideBivariate",
  GuideLegend,
  params = c(
    GuideLegend$params,
    list(
      key = NULL,
      n_breaks = NULL,
      prm_label = NULL,
      scd_label = NULL,
      prm_text = NULL,
      scd_text = NULL,
      size = NULL
    )
  ),
  draw = function(self, theme, params = self$params, ...) {
    draw_bivariate_key(
      key = params$key,
      size = params$size,
      n_breaks = params$n_breaks,
      prm_label = params$prm_label,
      scd_label = params$scd_label,
      prm_text = params$prm_text,
      scd_text = params$scd_text
    )
  }
)

guide_bivariate <- function(aesthetic,
                            value,
                            label,
                            n_breaks = c(4, 4),
                            prm_label = NULL,
                            scd_label = NULL,
                            prm_text = NULL,
                            scd_text = NULL,
                            size = NULL,
                            ...,
                            theme = NULL,
                            title = waiver(),
                            order = 0,
                            position = NULL) {
  key <- data.frame(aesthetic, .value = value, .label = label)

  new_guide(
    key = key,
    n_breaks = n_breaks,
    prm_label = prm_label,
    scd_label = scd_label,
    prm_text = prm_text,
    scd_text = scd_text,
    size = size,
    theme = theme,
    title = title,
    order = order,
    position = position,
    available_aes = 'fill',
    super = GuideBivariate
  )
}
