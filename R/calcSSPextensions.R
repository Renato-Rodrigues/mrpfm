#' Calculate SSP Extensions data
#'
#' @param subtype subtype "all" or "drivers_SSP2"
#'
#' @return A list with a [`magpie`][magclass::magclass] object, weight, unit and description.
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass getYears getNames mbind setNames time_interpolate
#'
calcSSPextensions <- function(subtype = "all") {
    if (subtype == "all") {
        raw <- readSource("SSPextensions")
    } else if (subtype == "drivers_SSP2") {
        variables <- c(
            "SSP2.Population|Urban [Share]",
            "SSP2.Gini Income Inequality Coefficient",
            "SSP2.Gender Inequality Index",
            "SSP2.Rule-of-Law Index",
            "SSP2.Governance Index|Government Effectiveness",
            "SSP2.Governance Index|Control of Corruption"
        )
        raw <- readSource("SSPextensions")[, , variables]
    } else {
        stop("Invalid subtype")
    }

    data <- collapseNames(raw, collapsedim = 3)
    data[data == 0] <- NA

    # fill missing years with interpolation
    data <- toolTimeInterpolation(data, interpolatedYears = c(seq(2000, 2024, 1), seq(2025, 2150, 5)))
    data <- toolImputeMedians(data)

    # Prepare weights. Only fetch the scenarios actually present in `data` (the weight loop
    # below indexes pop/gdp by the scenarios in data's 3rd dim): for the drivers_SSP2 subtype
    # that is SSP2 alone, so requesting all five SSPs would needlessly force the full mrdrivers
    # SSP scenario construction (raw SSP download). (ADR 0017.)
    wScenarios <- if (subtype == "drivers_SSP2") "SSP2" else c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
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

    # Define variables for each weight category
    varsSum <- c(
        "GDP|PPP [Conflict-Adjusted Projections]",
        "Net Migration",
        "Population|Extreme Poverty"
    )

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
