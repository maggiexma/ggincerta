#' Format input and assign the map class
#'
#' `duo()` returns a list of sub-lists, each containing paired values of the two
#' variables, along with their variable names stored as the map class.
#'
#' @param v1,v2 Two variables to be used in constructing the map.
#'
#' @examples
#' data(nc)
#' value <- nc$value; sd <- nc$sd
#' res <- duo(value, sd)
#' class(res)
#' attr(res, "vars")
#'
#' @rdname duo
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

#' @rdname duo
#' @export
duo_exceed <- function(estimate, error) {
  .v1 <- substitute(estimate)
  .v2 <- substitute(error)
  ind <- seq_along(estimate)
  structure(
    lapply(ind, function(i) list(v1 = estimate[i], v2 = error[i])),
    class = c("exceed", "list"),
    vars = c(.v1, .v2)
  )
}
