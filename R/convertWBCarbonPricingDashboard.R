#' Convert Read World Bank Carbon Pricing Dashboard data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readWBCarbonPricingDashboard()`].
#'
#' @param subtype character, type of data: price, priceAprilFirst, wbCoverage, revenue, emissionsCovered
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Renato Rodrigues
#'
#'
convertWBCarbonPricingDashboard <- function(x, subtype) {

  x[is.na(x)] <- 0 # setting NA values to zero
  x <- magpiesort(x)
  x <- madrat::toolFillYears(x, magclass::getYears(x, as.integer = TRUE))
  x <- madrat::toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
