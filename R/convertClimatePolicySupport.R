#' Convert climate policy support data to common country list
#'
#' Converts raw percent values to fractions [0, 1] and fills the complete
#' ISO country list (missing countries set to `NA`).
#'
#' @param x A [`magpie`][magclass::magclass] object returned from
#'   [`readClimatePolicySupport()`] with values in percent [0, 100].
#' @param subtype character; passed through (`"Vlasceanu2024"` or `"Andre2024"`).
#'
#' @return A [`magpie`][magclass::magclass] object with all ISO3c countries,
#'   values in the range [0, 1]. Missing countries are set to `NA`.
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat toolCountryFill
#'
convertClimatePolicySupport <- function(x, subtype) {
  # Convert percent → fraction
  x <- x / 100

  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2)

  return(x)
}
