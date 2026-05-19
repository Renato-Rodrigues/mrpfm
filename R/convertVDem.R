#' Convert V-Dem Governance Indicators
#'
#' Fills missing countries with `NA` to ensure a complete ISO country list.
#'
#' @param x A [`magpie`][magclass::magclass] object returned from
#'   [`readVDem()`].
#' @param subtype passed through; `"all"` or a V-Dem column code.
#'
#' @return A [`magpie`][magclass::magclass] object with all ISO3c countries
#'   (missing values set to `NA`).
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat toolCountryFill
#'
convertVDem <- function(x, subtype = "all") {
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 1)
  return(x)
}
