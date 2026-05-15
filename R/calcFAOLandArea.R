#' Calculate FAO Total Land Area
#'
#' Wraps [`readSource`] for FAO FAOSTAT land area data and returns it as a
#' `calcOutput` compatible magpie object.
#'
#' @description
#' Land area is constant in time. The function returns the most recent available
#' year's data. Missing countries are set to `NA`.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{x}{[`magpie`][magclass::magclass] object `[iso3c, year, "Land Area"]`}
#'     \item{weight}{`NULL`}
#'     \item{unit}{`"1000 ha"`}
#'     \item{description}{character description}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource
#'
calcFAOLandArea <- function() {
  x <- readSource("FAOLandArea")

  # Validate
  stopifnot(
    "Expected exactly 1 variable for FAOLandArea" = ndata(x) == 1,
    "Land area values must be non-negative" = all(x[!is.na(x)] >= 0)
  )
  return(list(
    x           = x,
    weight      = NULL,
    unit        = "1000 ha",
    description = "FAO FAOSTAT total land area per country (constant in time)"
  ))
}
