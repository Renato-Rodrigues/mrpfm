#' Calculate SSP Extensions data
#'
#' @param subtype subtype "all", or "drivers_SSP1" ... "drivers_SSP5": the six PFM driver series
#'   for one SSP. The Rule-of-Law Index is published for SSP1-3 only (Andrijevic et al. 2020);
#'   for SSP4 and SSP5 it takes the SSP2 path, relabelled, and says so.
#'
#' @return A list with a [`magpie`][magclass::magclass] object, weight, unit and description.
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass getYears getNames getNames<- mbind setNames time_interpolate
#'
calcSSPextensions <- function(subtype = "all") {
  driverVars <- c(
    "Population|Urban [Share]",
    "Gini Income Inequality Coefficient",
    "Gender Inequality Index",
    "Rule-of-Law Index",
    "Governance Index|Government Effectiveness",
    "Governance Index|Control of Corruption"
  )
  if (subtype == "all") {
    raw <- readSource("SSPextensions")
  } else if (grepl("^drivers_SSP[1-5]$", subtype)) {
    ssp <- sub("^drivers_", "", subtype)
    src <- readSource("SSPextensions")
    # A series counts as published for this SSP when it exists and holds any value.
    present <- vapply(driverVars, function(v) {
      s <- tryCatch(src[, , paste0(ssp, ".", v)], error = function(e) NULL)
      !is.null(s) && any(is.finite(s))
    }, logical(1))
    raw <- src[, , paste0(ssp, ".", driverVars[present])]
    if (any(!present)) {
      fb <- src[, , paste0("SSP2.", driverVars[!present])]
      getNames(fb, dim = 1) <- ssp
      raw <- mbind(raw, fb)
      message("calcSSPextensions: ", paste(driverVars[!present], collapse = ", "),
              " not published for ", ssp, "; using the SSP2 path.")
    }
    raw <- raw[, , paste0(ssp, ".", driverVars)]   # fixed variable order
  } else {
    stop("Invalid subtype")
  }

  data <- collapseNames(raw, collapsedim = 3)
  data[data == 0] <- NA

  # fill missing years with interpolation
  data <- toolTimeInterpolation(data, interpolatedYears = c(seq(2000, 2024, 1), seq(2025, 2150, 5)))
  data <- toolImputeMedians(data)

  # Prepare weights. Only fetch the scenarios actually present in `data` (the weight loop
  # below indexes pop/gdp by the scenarios in data's 3rd dim): for a drivers_SSPx subtype
  # that is SSPx alone, so requesting all five SSPs would needlessly force the full mrdrivers
  # SSP scenario construction (raw SSP download). (ADR 0017.)
  wScenarios <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
  if (grepl("^drivers_SSP[1-5]$", subtype)) wScenarios <- sub("^drivers_", "", subtype)
  # Population for everything related to people (shares, indices, etc.)
  pop <- calcOutput("Population", scenario = wScenarios, aggregate = FALSE)
  # GDP for economic shares
  gdp <- calcOutput("GDP", scenario = wScenarios, aggregate = FALSE)

  # Linear projection
  yearsData <- getYears(data, as.integer = TRUE)
  pop <- time_interpolate(pop,
    interpolated_year = yearsData,
    integrate_interpolated_years = TRUE, extrapolation_type = "linear"
  )
  gdp <- time_interpolate(gdp,
    interpolated_year = yearsData,
    integrate_interpolated_years = TRUE, extrapolation_type = "linear"
  )

  # Common years
  years <- intersect(getYears(data), intersect(getYears(pop), getYears(gdp)))
  data <- data[, years, ]
  pop <- pop[, years, ]
  gdp <- gdp[, years, ]

  # Define variables for each weight category. Extensive variables ("GDP|PPP [Conflict-Adjusted
  # Projections]", "Net Migration", "Population|Extreme Poverty") keep an NA weight, which
  # mixed_aggregation sums.

  varsPop <- c(
    # Governance & Rule of Law:
    "Governance Index",
    "Governance Index|Control of Corruption",
    "Governance Index|Government Effectiveness",
    "Rule-of-Law Index",
    # Social & Development Indicators:
    "Human Development Index",
    "Gender Inequality Index",
    "Probability of Armed Conflict",
    "Population|Urban UNDP [Share]", "Population|Urban [Share]",
    "Population|Extreme Poverty [Share]",
    # Income Distribution & Inequality:
    "Income Distribution|1st Decile", "Income Distribution|2nd Decile", "Income Distribution|3rd Decile",
    "Income Distribution|4th Decile", "Income Distribution|5th Decile", "Income Distribution|6th Decile",
    "Income Distribution|7th Decile", "Income Distribution|8th Decile", "Income Distribution|9th Decile",
    "Income Distribution|10th Decile",
    "Gini Income Inequality Coefficient",
    "Net Remittances [per capita]",
    # Labor Market:
    "Employment|Agriculture [Share]", "Employment|Industry [Share]", "Employment|Services [Share]"
  )

  varsGDP <- c(
    # Economic Composition:
    "Value Added|Agriculture [Share]", "Value Added|Industry [Share]", "Value Added|Services [Share]"
  )

  # Construct weight object
  weight <- data
  weight[, , ] <- NA
  for (SSP in getNames(data, dim = 1)) {
    if (length(intersect(getNames(data, dim = 2), varsPop)) > 0) {
      weight[, , paste0(SSP, ".", intersect(getNames(data, dim = 2), varsPop))] <- pop[, , SSP]
    }
    if (length(intersect(getNames(data, dim = 2), varsGDP)) > 0) {
      weight[, , paste0(SSP, ".", intersect(getNames(data, dim = 2), varsGDP))] <- gdp[, , SSP]
    }
  }

  return(list(
    x = data,
    weight = weight,
    mixed_aggregation = TRUE,
    unit = "various",
    description = "SSP Extensions data"
  ))
}
