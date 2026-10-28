#' Convert World Bank fossil rents
#'
#' Fills the ISO country list with `NA` (countries without a World Bank series stay missing).
#'
#' @param x A [`magpie`][magclass::magclass] object from [readWDIRents()].
#' @return The same object on the full ISO country list.
#' @author Renato Rodrigues
#' @importFrom madrat toolCountryFill
convertWDIRents <- function(x) {
  madrat::toolCountryFill(x, fill = NA, verbosity = 2)
}
