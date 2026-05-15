#' Convert World Bank Worldwide Governance Indicators
#'
#' Fills missing countries and returns a complete ISO country list.
#'
#' @param x A [`magpie`][magclass::magclass] object returned from
#'   [`readWGIindicator()`].
#' @param subtype passed through; one of the WGI indicator codes or `"all"`.
#'
#' @return A [`magpie`][magclass::magclass] object with all ISO3c countries
#'   (missing values set to `NA`).
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat toolCountryFill
#'
convertWGIindicator <- function(x, subtype = "all") {
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2)

  return(x)
}
