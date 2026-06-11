#' Calculate V-Dem Governance Indicators
#'
#' Wraps [`readSource`] for V-Dem data and returns governance indicators as a
#' single magpie object for use as a `calcOutput`.
#'
#' @description
#' Default (`subtype = "all"`) returns four accountability indicators:
#' Rule of Law (VDem), Vertical Accountability (VDem),
#' Horizontal Accountability (VDem), Diagonal Accountability (VDem).
#' All on a 0–1 scale; higher means better governance / stronger accountability.
#'
#' `subtype = "stateCapacity"` returns five state-capacity indicators that serve
#' as V-Dem replacements for WGI Government Effectiveness:
#' Civil Service Professionalism, Executive Corruption, Rule Predictability,
#' Political Corruption, Neopatrimonialism.
#' Note: three of these are "bad when high" — they are inverted in
#' `panelDataHistorical()` before normalisation.
#'
#' Pass any V-Dem column code as `subtype` to retrieve a single arbitrary
#' indicator (e.g. `calcOutput("VDem", subtype = "v2x_corr")`).
#' Use [`listVDemIndicators()`] to enumerate available codes.
#'
#' @param subtype character; `"all"` (default), `"stateCapacity"`, or a V-Dem column code.
#'
#' @return A list with:
#'   \describe{
#'     \item{x}{[`magpie`][magclass::magclass] object `[iso3c, year, indicator]`}
#'     \item{weight}{SSP2 population weights for spatial aggregation}
#'     \item{unit}{`"index (0-1)"`}
#'     \item{description}{character description}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass getYears getNames time_interpolate ndata
#'
calcVDem <- function(subtype = "all") {
  x <- readSource("VDem", subtype = subtype, convert = TRUE)
  x <- toolImputeMedians(x)

  pop <- tryCatch(
    suppressWarnings(calcOutput("Population", scenario = "SSP2", aggregate = FALSE)),
    error = function(e) {
      NULL
    }
  )

  if (!is.null(pop)) {
    yearsData <- getYears(x, as.integer = TRUE)
    pop <- time_interpolate(pop,
      interpolated_year = yearsData,
      integrate_interpolated_years = TRUE, extrapolation_type = "linear"
    )

    years <- intersect(getYears(x), getYears(pop))
    x   <- x[, years, ]
    pop <- pop[, years, ]

    weight <- x
    weight[, , ] <- NA
    # pop might have scenario names or just be 1D
    popName <- if ("SSP2" %in% getNames(pop)) "SSP2" else getNames(pop)[[1]]
    weight[, , getNames(x)] <- pop[, , popName]
  } else {
    weight <- x
    weight[, , ] <- 1
  }

  nIndicators <- if (subtype == "all")            4L
                 else if (subtype == "stateCapacity") 5L
                 else                                 1L
  stopifnot("Unexpected number of V-Dem indicators" = ndata(x) == nIndicators)

  return(list(
    x           = x,
    weight      = weight,
    unit        = "index (0-1)",
    description = "V-Dem governance indicators for the Political Feasibility Module (PFM)"
  ))
}
