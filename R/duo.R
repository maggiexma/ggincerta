#' Format input and assign the "bivariate" class
#'
#' `duo()` returns a list of sub-lists, each containing paired values of the two
#' variables, along with their variable names stored as the `"bivariate"` class.
#'
#' @param v1,v2 Two variables to be used in constructing the bivariate map.
#'
#' @examples
#' data(nc)
#' value <- nc$value; sd <- nc$sd
#' res <- duo(value, sd)
#' class(res)
#' attr(res, "vars")
#'
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

duo_exceed <- function(estimate, error) {
  browser()
  .v1 <- substitute(estimate)
  .v2 <- substitute(error)
  ind <- seq_along(estimate)
  structure(
    lapply(ind, function(i) list(v1 = estimate[i], v2 = error[i])),
    class = c("exceed", "list"),
    vars = c(.v1, .v2)
  )
}

exceed <- function(..., dist_fun, threshold = NA) {
  structure(list(args = list(...)),
            threshold = threshold,
            dist_fun = dist_fun,
            class = c("exceed", "list"))

  dist_fun(!!args)

}
