#' Calculate Carbon Price value and emisssions coverage based on the World Bank dashboard data
#'
#' Carbon Price data is calculated from World Bank carbon price, World Bank carbon price coverage shares,
#' and EDGAR Global GHG Emissions for sectoral disaggregation.
#'
#' @param subtype data subtype. Either "emissionsCovered", "shareEmissionsCovered", "carbonPrice", "effectivePrice"
#' @returns MAgPIE object with Carbon Price data per country and sector group
#'
#' @author Renato Rodrigues
#'
#' @examples
#' \dontrun{
#' calcOutput("CarbonPrice", subtype = "effectivePrice")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr rename select mutate left_join across filter %>% bind_rows .data
#'
calcCarbonPrice <- function(subtype = "effectivePrice") {
  priceRaw <- madrat::readSource("WBCarbonPricingDashboard", subtype = "price")

  priceFull <- magclass::dimReduce(magclass::dimSums(magclass::mselect(priceRaw, status = "implemented"),
    3.1,
    na.rm = TRUE
  ))

  pricePerSectorGroup <-
    suppressWarnings(mbind(
      magclass::mselect(priceFull, sector_group = "bulk")
      + setNames(magclass::mselect(priceFull, sector_group = "all"), "bulk"),
      magclass::mselect(priceFull, sector_group = "diffuse")
      + setNames(magclass::mselect(priceFull, sector_group = "all"), "diffuse"),
      magclass::mselect(priceFull, sector_group = "bunkers")
    ))

  wbEmissionsCoveredRaw <- madrat::readSource("WBCarbonPricingDashboard", subtype = "emissionsCovered")
  wbEmissionsCovered <-
    magclass::dimReduce(magclass::dimSums(magclass::mselect(wbEmissionsCoveredRaw, status = "implemented"),
      3.1,
      na.rm = TRUE
    ))

  # historical sector group emissions
  bulk <- c("Industrial Combustion", "Power Industry", "Processes", "Fuel Exploitation")
  diffuse <- c("Buildings", "Transport", "Agriculture", "Waste")
  bunkers <- c("Aviation", "Shipping")
  histEmiRaw <- madrat::readSource("EDGARghg")

  # copying last year for missing years
  missYears <- setdiff(c(2000:2025), getYears(histEmiRaw, as.integer = TRUE))
  for (y in missYears) {
    tmp <- histEmiRaw[, max(getYears(histEmiRaw, as.integer = TRUE)), ]
    magclass::getYears(tmp) <- y
    histEmiRaw <- magclass::mbind(histEmiRaw, tmp)
  }
  histEmi <- histEmiRaw[, getYears(wbEmissionsCovered), ]
  histEmiBulk <- setNames(magclass::dimSums(magclass::mselect(histEmi, variable = bulk), 3, na.rm = TRUE), "bulk")
  histEmiDiffuse <- setNames(
    magclass::dimSums(magclass::mselect(histEmi, variable = diffuse), 3, na.rm = TRUE),
    "diffuse"
  )
  histEmiBunkers <- setNames(
    magclass::dimSums(magclass::mselect(histEmi, variable = bunkers), 3, na.rm = TRUE),
    "bunkers"
  )
  histEmiPerSectorGroup <- mbind(histEmiBulk, histEmiDiffuse, histEmiBunkers)
  histEmiPerSectorGroupFiltered <- magclass::mselect(histEmiPerSectorGroup, d3 = c("bulk", "diffuse"))

  shareHistEmiSectorGroupPerCountryEmi <-
    histEmiPerSectorGroupFiltered / magclass::dimSums(histEmiPerSectorGroupFiltered, 3)

  # split emissions applied to all economy into sector groups
  splitAllSectorGorupEmi <-
    shareHistEmiSectorGroupPerCountryEmi *
      magclass::dimReduce(magclass::mselect(wbEmissionsCovered, sector_group = "all"))
  magclass::getSets(splitAllSectorGorupEmi)["d3.1"] <- "sector_group"
  expost1EmissionsCovered <- magclass::mselect(wbEmissionsCovered, sector_group = c("bulk", "diffuse", "bunkers"))
  expost2EmissionsCovered <- suppressWarnings(mbind(
    magclass::mselect(expost1EmissionsCovered, sector_group = c("bulk", "diffuse"))
    + magclass::mselect(splitAllSectorGorupEmi, sector_group = c("bulk", "diffuse")),
    magclass::mselect(expost1EmissionsCovered,
      sector_group = setdiff(magclass::getNames(expost1EmissionsCovered), c("bulk", "diffuse"))
    )
  ))

  # ex-post adjustments to the emissions covered data (emissionsCovered)
  # remove LIE (Liechtenstein) as it has no data in the EDGAR historical emissions,
  #     MNE (Montenegro) as it only has it only has bunkers data in the EDGAR historical emissions, and
  #     AND (Andorra) as it is not representative and it has too high coverage in the world bank carbon price dashboard
  expost2EmissionsCovered[c("LIE", "MNE", "AND"), , ] <- 0

  # if a country has more emissions in a sector group than historical data, move the difference to other sector group
  #     bulk to diffuse for NZL (New Zealand) and CHL (Chile)
  #     diffuse to bulk for NOR (Norway), DNK (Denmark), FIN (Finland) and SVN (Slovenia)
  diff <- quitte::as.quitte(expost2EmissionsCovered - histEmiPerSectorGroupFiltered) %>%
    select(c("region", "period", "sector_group", "value")) %>%
    filter(.data$value > 0)
  expost3EmissionsCovered <- as.quitte(expost2EmissionsCovered) %>%
    select(c("region", "period", "sector_group", "value")) %>%
    left_join(bind_rows(
      diff %>% filter(.data$sector_group == "bulk") %>% mutate(value_fromBulk = -.data$value) %>% select(-c("value")), # nolint
      diff %>%
        filter(.data$sector_group == "bulk") %>%
        mutate(sector_group = "diffuse") %>%
        rename(value_fromBulk = .data$value) # nolint
    ), by = c("region", "period", "sector_group")) %>%
    left_join(
      bind_rows(
        diff %>% filter(.data$sector_group == "diffuse") %>% mutate(value_fromDiffuse = -.data$value) %>% select(-c("value")), # nolint
        diff %>%
          filter(.data$sector_group == "diffuse") %>%
          mutate(sector_group = "bulk") %>%
          dplyr::rename(value_fromDiffuse = .data$value)
      ), # nolint
      by = c("region", "period", "sector_group")
    ) %>%
    mutate(across(where(is.numeric), ~ coalesce(.x, 0)),
      value = .data$value + .data$value_fromBulk + .data$value_fromDiffuse
    ) %>%
    select(c("region", "period", "sector_group", "value")) %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value") %>%
    suppressWarnings()

  # sector_groups with more emissions than in the historical data after all fixes are set to match historical data
  # diff2 <- quitte::as.quitte(expost3EmissionsCovered - histEmiPerSectorGroupFiltered) %>% select(c("region", "period", "sector_group", "value")) %>% filter(.data$value > 0) # nolint
  emissionsCovered <- pmin(expost3EmissionsCovered, histEmiPerSectorGroupFiltered)

  # share of covered emissions per sector group
  shareEmissionsCoveredPerSectorGroup <- emissionsCovered / histEmiPerSectorGroupFiltered
  shareEmissionsCoveredPerSectorGroup[is.nan(shareEmissionsCoveredPerSectorGroup)] <- 0

  # carbon price
  carbonPrice <-
    pricePerSectorGroup[
      , intersect(magclass::getYears(pricePerSectorGroup), magclass::getYears(emissionsCovered)),
      intersect(magclass::getNames(pricePerSectorGroup), magclass::getNames(emissionsCovered))
    ]

  # effective carbon price
  effectivePrice <- magclass::dimReduce(carbonPrice *
    shareEmissionsCoveredPerSectorGroup[
      , intersect(
        magclass::getYears(carbonPrice), # nolint
        magclass::getYears(emissionsCovered)
      ),
      intersect(
        magclass::getNames(carbonPrice),
        magclass::getNames(emissionsCovered)
      )
    ])

  # GDP per capita
  pop <- calcOutput("PopulationPast", aggregate = FALSE)
  gdp <- calcOutput("GDPPast", aggregate = FALSE)
  gdpPerCapita <- magclass::collapseNames(gdp[, intersect(getYears(pop), getYears(gdp)), ] /
    pop[, intersect(getYears(pop), getYears(gdp)), ])
  # copying last year for missing years
  tmp <- tmp2 <- gdpPerCapita[, 2023, ]
  magclass::getYears(tmp) <- 2024
  magclass::getYears(tmp2) <- 2025
  gdpPerCapita <- magclass::mbind(gdpPerCapita, tmp, tmp2)

  switch(subtype, # nolint
    "emissionsCovered" = { # nolint
      data <- emissionsCovered
      weight <- NULL
      dataDescription <- "Emissions covered by carbon price instruments in a given country and a sector group"
      dataUnit <- "Mt CO2"
    }, # nolint
    "shareEmissionsCovered" = { # nolint
      data <- shareEmissionsCoveredPerSectorGroup
      weight <-
        histEmiPerSectorGroup[, getYears(shareEmissionsCoveredPerSectorGroup), getNames(shareEmissionsCoveredPerSectorGroup)] # nolint
      dataDescription <- "Share of emissions covered by carbon price instruments in a given country and a sector group" # nolint
      dataUnit <- "fraction"
    }, # nolint
    "carbonPrice" = { # nolint
      data <- carbonPrice
      weight <- histEmiPerSectorGroup[, getYears(carbonPrice), getNames(carbonPrice)]
      dataDescription <- "Carbon price per sector group based on the World Bank dashboard data"
      dataUnit <- "US$2017/t CO2"
    }, # nolint
    "effectivePrice" = { # nolint
      data <- effectivePrice
      weight <- histEmiPerSectorGroup[, getYears(effectivePrice), getNames(effectivePrice)]
      dataDescription <- "Effective carbon price per sector group based on the World Bank and historical data"
      dataUnit <- "US$2017/t CO2"
    }
  ) # nolint

  return(list(
    x = data,
    weight = weight,
    min = 0,
    description = dataDescription,
    unit = dataUnit
  ))
}
