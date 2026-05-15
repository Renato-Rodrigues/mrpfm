#' Convert SSP Extensions data
#'
#' @param x A [`magpie`][magclass::magclass] object returned from [`readSSPextensions()`].
#' @return A [`magpie`][magclass::magclass] object.
#' @author Renato Rodrigues
#'
#' @importFrom madrat toolFillYears toolCountryFill
#' @importFrom magclass getYears
#'
convertSSPextensions <- function(x) {
  x <- x[, sort(magclass::getYears(x, as.integer = TRUE)), ]
  x <- madrat::toolFillYears(x, sort(magclass::getYears(x, as.integer = TRUE)))
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2)

  return(x)
}
