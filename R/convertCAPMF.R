#' Convert OECD Climate Actions and Policies Measurement Framework (CAPMF) data
#'
#' Fills missing countries with `NA` to ensure a complete ISO country list.
#'
#' @param x A [`magpie`][magclass::magclass] object returned from [`readCAPMF()`].
#' @param subtype passed through; "all" or specific sector.
#'
#' @return A [`magpie`][magclass::magclass] object with all ISO3c countries (missing values set to `NA`).
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat toolCountryFill
#'
#' @export
convertCAPMF <- function(x, subtype = "all") {
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 1)
  return(x)
}
