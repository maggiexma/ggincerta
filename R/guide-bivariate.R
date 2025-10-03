#' @export
GuideBivariate <- ggproto(
  "GuideBivariate",
  GuideLegend,
  params = c(
    GuideLegend$params,
    list(
      key = NULL,
      est_label = NULL,
      err_label = NULL,
      est_text = NULL,
      err_text = NULL,
      size = NULL
    )
  ),
  draw = function(self, theme, params = self$params, ...) {
    draw_bivariate_key(
      key = params$key,
      size = params$size,
      est_label = params$est_label,
      err_label = params$err_label,
      est_text = params$est_text,
      err_text = params$err_text
    )
  }
)

guide_bivariate <- function(aesthetic,
                            value,
                            label,
                            est_label = NULL,
                            err_label = NULL,
                            est_text = NULL,
                            err_text = NULL,
                            size = NULL,
                            ...,
                            theme = NULL,
                            title = waiver(),
                            order = 0,
                            position = NULL) {
  key <- data.frame(aesthetic, .value = value, .label = label)

  new_guide(
    key = key,
    est_label = est_label,
    err_label = err_label,
    est_text = est_text,
    err_text = err_text,
    size = size,
    theme = theme,
    title = title,
    order = order,
    position = position,
    available_aes = 'fill',
    super = GuideBivariate
  )
}
