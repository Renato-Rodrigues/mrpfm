#' Convert Global economy data indicators
#' aggregation of over 500 indicators for over 200 countries from 1960 - 2024.
#' sources: central banks, national statistical institutes, and multiple international organizations.
#' https://www.theglobaleconomy.com/
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readWBCarbonPricingDashboard()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Renato Rodrigues
#'
#'
convertGlobalEconomyData <- function(x) {
  x <- magpiesort(x)
  x <- madrat::toolFillYears(x, sort(magclass::getYears(x, as.integer = TRUE)))
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2)

  return(x)
}
