#' @export
duo <- function(v1, v2) {
  .v1 = substitute(v1)
  .v2 = substitute(v2)
  ind <- seq_along(v1)
  structure(
    lapply(ind, function(i) list(v1 = v1[i], v2 = v2[i])),
    class = c("bivariate", "list"),
    vars = c(.v1, .v2)
  )
}
