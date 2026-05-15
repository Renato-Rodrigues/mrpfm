#' Calculate Worldwide Governance Indicators
#'
#' Wraps [`readSource`] for WGI indicator data and returns all six governance
#' indicators as a single magpie object suitable for use as a `calcOutput`.
#'
#' @description
#' Returns all six WGI governance indicators:
#' Voice and Accountability, Political Stability, Government Effectiveness,
#' Regulatory Quality, Rule of Law, Control of Corruption.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{x}{[`magpie`][magclass::magclass] object `[iso3c, year, indicator]`}
#'     \item{weight}{`NULL` (indicators are already country-level index values)}
#'     \item{unit}{`"index"`}
#'     \item{description}{character description}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass getYears getNames mbind setNames time_interpolate ndata
#'
calcWGIindicator <- function() {
  x <- readSource("WGIindicator", subtype = "all", convert = TRUE)
  x[is.na(x)] <- 0

  # Prepare weights
  # Population for governance indicators spatial scaling
  pop <- calcOutput("Population", scenario = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"), aggregate = FALSE)

  # Linear projection for intermediate years
  yearsData <- getYears(x, as.integer = TRUE)
  pop <- time_interpolate(pop,
    interpolated_year = yearsData,
    integrate_interpolated_years = TRUE, extrapolation_type = "linear"
  )

  # Common years
  years <- intersect(getYears(x), getYears(pop))
  x <- x[, years, ]
  pop <- pop[, years, ]

  # Construct weight object
  weight <- x
  weight[, , ] <- NA
  weight[, , getNames(x)] <- pop[, , "SSP2"]

  # Validate
  stopifnot("Expected exactly 6 WGI indicators" = ndata(x) == 6)
  return(list(
    x           = x,
    weight      = weight,
    unit        = "index",
    description = "World Bank Worldwide Governance Indicators for the Political Feasibility Module (PFM)"
  ))
}
