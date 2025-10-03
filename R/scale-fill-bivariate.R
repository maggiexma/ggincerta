#' @export
ScaleFillBivar <- ggproto(
  "ScaleFillBivar",
  ScaleDiscrete,
  aesthetics = "fill",
  drop = FALSE,
  na.value = NA,

  train_df = function(self, df, ...) {
    if ("fill" %in% names(df)) {
      info <- attr(df$fill, "bivar_breaks", exact = TRUE)
      if (!is.null(info)) {
        self$est_breaks <- info$est_breaks
        self$err_breaks <- info$err_breaks
        self$n_est <- info$n_est %||% 3L
        self$n_err <- info$n_err %||% 3L
      }
    }

    ggproto_parent(ScaleDiscrete, self)$train_df(df, ...)

    n <- as.integer((self$n_est %||% 3L) * (self$n_err %||% 3L))
    cols <- self$palette(n)
    if (length(cols) < n)
      cols <- rep_len(cols, n)
    self$legend_cols <- unname(as.character(cols))

    self$guide <- guide_bivariate(
      aesthetic = setNames(list(self$legend_cols), "fill"),
      value = as.character(seq_len(n)),
      label = as.character(seq_len(n)),
      est_label = format(self$est_breaks, digits = 2),
      err_label = format(self$err_breaks, digits = 2),
      est_text = self$est_text %||% "primary",
      err_text = self$err_text %||% "secondary",
      size = self$guide_size
    )

    invisible()
  }
)

scale_fill_bivariate <- function(name = waiver(),
                                 name_primary = waiver(),
                                 name_secondary = waiver(),
                                 colors = c("gold", "red4"),
                                 n_breaks = c(4, 4),
                                 blend = c("additive", "subtractive"),
                                 flip = c("none", "vertical", "horizontal", "both"),
                                 guide_size = 1.5,
                                 na.value = NA,
                                 na.translate = TRUE,
                                 ...) {
  flip <- match.arg(flip)
  blend <- match.arg(blend)

  pal_fun <- bivar_palette(colors[1],
                           colors[2],
                           n_breaks_primary = n_breaks[1],
                           n_breaks_secondary = n_breaks[2],
                           blend = blend,
                           flip = flip)
  pal_safe <- function(n)
    pal_fun(as.integer(n)[1])

  sc <- discrete_scale(
    aesthetics = "fill",
    palette = pal_safe,
    name = name,
    guide = "legend",
    drop = FALSE,
    na.value = na.value,
    na.translate = na.translate,
    super = ScaleFillBivar,
    ...
  )
  sc$est_text <- name_primary
  sc$err_text <- name_secondary
  sc$guide_size <- guide_size
  sc
}
