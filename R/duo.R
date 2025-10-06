#' @export
duo <- function(v1, v2) {
  ind <- seq_along(v1)
  structure(lapply(ind, function(i) list(v1 = v1[i], v2 = v2[i])),
            class = c("bivariate", "list"))
}
