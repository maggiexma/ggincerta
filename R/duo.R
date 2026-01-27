#' Format input and assign the "map" class
#'
#' `duo()` and `duo_exceed()` create paired mapping objects that combine two
#' variables, record their names, and assign the bivariate/exceed class as
#' an attribute for use in aesthetic mappings.
#'
#' @param v1,v2 Input variables for `duo()`.
#' @param estimate,error Input variables for `duo_exceed()` representing the
#'   point estimate and its uncertainty.
#'
#' @returns A list-like object containing pairs of values from the two variables,
#'   with attributes storing the variable names and the class.
#'
#' @examples
#' value <- nc$value
#' sd <- nc$sd
#' res <- duo(value, sd)
#' res_exceed <- duo_exceed(value, sd)
#' class(res); class(res_exceed)
#' attr(res, "vars"); attr(res_exceed, "vars")
#'
#' @name duo
#' @aliases duo duo_exceed
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

duo_pixel <- function(estimate, error) {
  .v1 <- substitute(estimate)
  .v2 <- substitute(error)
  ind <- seq_along(estimate)
  structure(
    lapply(ind, function(i)
      list(v1 = estimate[i], v2 = error[i])),
    class = c("pixel", "list"),
    vars  = c(.v1, .v2)
  )
}
