#' Calculate Climate Policy Support indicators
#'
#' Combines both climate policy support survey datasets
#' (Vlasceanu 2024 and Andre 2024) into a single magpie object.
#'
#' @description
#' Returns two variables:
#' \itemize{
#'   \item `"Support policies climate"` — Vlasceanu et al., 2024, *Science*
#'   \item `"Support political climate action"` — Andre et al., 2024, *Nature Climate Change*
#' }
#' Values are fractions in [0, 1] (converted from percent in the read/convert step).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{x}{[`magpie`][magclass::magclass] object `[iso3c, year, variable]`}
#'     \item{weight}{`NULL` (country-level survey values, no aggregation weight needed)}
#'     \item{unit}{`"fraction"`}
#'     \item{description}{character description}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource toolFillYears
#' @importFrom magclass mbind getYears
#'
calcClimatePolicySupport <- function() {
  vlasceanu <- readSource("ClimatePolicySupport", subtype = "Vlasceanu2024", convert = TRUE)
  andre <- readSource("ClimatePolicySupport", subtype = "Andre2024", convert = TRUE)

  # Align temporal dimension: fill both to the union of their years
  allYears <- union(getYears(vlasceanu), getYears(andre))
  vlasceanu <- toolFillYears(vlasceanu, allYears)
  andre <- toolFillYears(andre, allYears)

  x <- magclass::mbind(vlasceanu, andre)

  stopifnot(
    "Climate policy support fractions must be <= 1" = all(x[!is.na(x)] <= 1),
    "Expected exactly 2 survey variables (Andre, Vlasceanu)" = ndata(x) == 2,
    "Climate policy support fractions must be >= 0" = all(x[!is.na(x)] >= 0)
  )

  return(list(
    x           = x,
    weight      = NULL,
    unit        = "fraction",
    description = "Public climate policy support data for the Political Feasibility Module (PFM)"
  ))
}
