#' Format input and assign the map class
#'
#' `duo()` and `duo_exceed()` create a paired mapping object that combines
#' two variables, records their names, and assigns the `"map"` class as an
#' attribute for use in aesthetic mappings.
#'
#' @param v1,v2,estimate,error Input variables for constructing paired mappings.
#'   `duo()` uses `v1` and `v2`, whereas `duo_exceed()` uses `estimate` and
#'   `error` to represent the point and uncertainty measures, respectively.
#'
#' @returns A list of sub-lists, each containing a pair of values from the two
#' variables, with attributes storing the variable names and the `"map"` class.
#'
#' @examples
#' value <- nc$value
#' sd <- nc$sd
#'
#' # Create a paired mapping object using duo()
#' res <- duo(value, sd)
#' res_exceed <- duo_exceed(value, sd)
#' class(res)
#' class(res_exceed)
#' attr(res, "vars")
#' attr(res_exceed, "vars")
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
