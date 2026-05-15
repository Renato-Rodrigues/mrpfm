#' Read FAO Total Land Area
#'
#' Reads FAOSTAT land area data from an Excel file in the `FAOLandArea` source
#' folder. Returns a constant-in-time magpie object with land area per country
#' in 1000 ha.
#'
#' @description
#' The FAO FAOSTAT "Inputs: Land Use - Land Cover" file contains country names
#' in the `Area` column and numeric land area values in the `Value` column.
#' Country names are converted to ISO 3166-1 alpha-3 codes using the
#' `countrycode` package.
#'
#' @return A [`magpie`][magclass::magclass] object with dimensions
#'   `[iso3c, year, "Land Area"]` in units of 1000 ha.
#'
#' @author Renato Rodrigues
#'
#' @importFrom readxl read_excel
#' @importFrom magclass as.magpie getSets<- new.magpie getRegions
#' @importFrom utils read.csv
#'
readFAOLandArea <- function() {
  raw <- suppressMessages(readxl::read_excel("./land_area.xlsx"))

  df <- raw %>%
    mutate(iso3 = countrycode::countrycode(.data$Area,
      origin = "country.name",
      destination = "iso3c", warn = FALSE
    )) %>%
    filter(!is.na(.data$iso3)) %>%
    mutate(year = 2023) %>%
    select(region = .data$iso3, .data$year, land_area = .data$Value)

  # isoCountry <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL) # nolint
  # missingCountries <- setdiff(isoCountry$x, df$region) # nolint
  # isoCountry %>% filter(x %in% missingCountries) # nolint

  out <- df %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "land_area")

  return(out)
}
