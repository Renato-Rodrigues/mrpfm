#' Convert FAO Land Area data to complete ISO country list
#'
#' @param x A [`magpie`][magclass::magclass] object returned from
#'   [`readFAOLandArea()`].
#'
#' @return A [`magpie`][magclass::magclass] object with all ISO3c countries
#'   (missing values set to `NA`).
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat toolCountryFill
#'
convertFAOLandArea <- function(x) {
  x <- madrat::toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
