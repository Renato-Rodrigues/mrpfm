#' Read World Bank Carbon Pricing Dashboard data
#'
#' This data can be downloaded directly from the site.
#'
#' @param subtype character, type of data: price, priceAprilFirst, wbCoverage, revenue,
#'   emissionsCovered, or the subnational variants priceSubnational / emissionsCoveredSubnational
#'   (within_country instruments mapped to their parent country, instrument identity kept)
#' @returns MagPIE object with world banck data on carbon price per country
#'
#' @author Renato Rodrigues
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select all_of matches mutate left_join across filter %>% rename_with join_by group_by
#' bind_rows case_when .data summarize coalesce
#' @importFrom tidyr extract pivot_longer
#' @importFrom quitte as.quitte
#'
readWBCarbonPricingDashboard <- function(subtype = "price") {

  dashboardFile <- "data_2025.xlsx"

  rawInfo <- suppressMessages(
    readxl::read_excel(dashboardFile, sheet = "Compliance_Gen Info", skip = 1, .name_repair = "unique")
  ) %>%
    rename_with(~gsub(" ", "_", tolower(.x)))
  rawEmissions <- readxl::read_excel(dashboardFile, sheet = "Compliance_Emissions", skip = 2) %>%
    rename_with(~gsub(" ", "_", tolower(.x)))
  rawRevenue <- readxl::read_excel(dashboardFile, sheet = "Compliance_Revenue", skip = 1) %>%
    rename_with(~gsub(" ", "_", tolower(.x)))
  rawPrice <- readxl::read_excel(dashboardFile, sheet = "Compliance_Price", skip = 1) %>%
    rename_with(~gsub(" ", "_", tolower(.x)))

  wbRegionMapping <- toolGetMapping("wbRegion.csv", type = "regional", where = "mrpfm")
  wbSectoralMapping <- toolGetMapping("wbSector.csv", type = "sectoral", where = "mrpfm")

  # Flag-derived sector_group fallback (ADR 0016): for instruments NOT classified in
  # wbSector.csv (empty/NA sector_group), derive it from the dashboard's own per-sector
  # coverage flags ("Yes"/"In principle"). The manual mapping stays authoritative where
  # present, so existing (implemented) classifications — and the default outputs — are
  # unchanged; this only auto-classifies new/unmapped instruments in future vintages.
  if ("sector_group" %in% names(wbSectoralMapping)) {
    isCov <- function(col) if (is.null(col)) FALSE else (!is.na(col) & col %in% c("Yes", "In principle"))
    fcol <- function(nm) if (nm %in% names(rawInfo)) rawInfo[[nm]] else NULL
    bulkF <- isCov(fcol("electricity_and_heat")) | isCov(fcol("industry")) | isCov(fcol("mining_and_extractives"))
    diffF <- isCov(fcol("transport")) | isCov(fcol("buildings")) |
      isCov(fcol("agriculture,_forestry_and_fishing_fuel_use")) | isCov(fcol("waste"))
    bunkF <- isCov(fcol("aviation"))
    derived <- data.frame(unique_id = rawInfo$unique_id, derived_sg = dplyr::case_when(
      bulkF & diffF ~ "all", bulkF ~ "bulk", diffF ~ "diffuse", bunkF ~ "bunkers", TRUE ~ NA_character_),
      stringsAsFactors = FALSE)
    wbSectoralMapping <- wbSectoralMapping %>%
      dplyr::left_join(derived, by = "unique_id") %>%
      dplyr::mutate(sector_group = dplyr::if_else(is.na(.data$sector_group) | .data$sector_group == "",
                                                  .data$derived_sg, .data$sector_group)) %>%
      dplyr::select(-"derived_sg")
  }

  metadata <- rawInfo  %>%
    left_join(wbRegionMapping, by = "unique_id") %>%
    left_join(wbSectoralMapping, by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "instrument_name", "type", "status", "jurisdiction_covered",
                    "gases_covered", "sector_group", "electricity_and_heat", "industry",
                    "mining_and_extractives", "transport", "aviation", "buildings",
                    "agriculture,_forestry_and_fishing_fuel_use", "agricultural_emissions", "waste", "lulucf",
                    "fuels_covered", "allocation_approaches", "price_or_market_management", "point_of_regulation",
                    "offset_eligibility", "description", "recent_developments", "coverage", "pricing_and_allocation",
                    "compliance", "relation_to_other_instruments")) %>%
    dplyr::left_join(rawPrice %>% dplyr::select(c("unique_id", "income_group")), by = "unique_id") %>% # nolint
    dplyr::mutate(type = case_when(type == "Carbon tax" ~ "carbon_tax",
                                   type == "ETS"        ~ "ets",
                                   type == "Undecided"  ~ NA,
                                   type == "Carbon Tax" ~ "carbon_tax"),
                  status = case_when(status == "Implemented"         ~ "implemented",
                                     status == "Under consideration" ~ "under_consideration",
                                     status == "Abolished"           ~ "abolished",
                                     status == "Under development"   ~ "under_development"))

  wbCoverage <- rawInfo  %>%
    left_join(wbRegionMapping, by = "unique_id") %>%
    left_join(wbSectoralMapping, by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "sector_group",
                    "share_of_jurisdiction_emissions_covered")) %>%
    tidyr::extract(
      .data$share_of_jurisdiction_emissions_covered,
      into = c("emissions_coverage", "global_emissions"),
      regex = "(\\d+\\.?\\d*)% of jurisdiction emissions, (\\d+\\.?\\d*)% of global emissions",
      remove = TRUE
    ) %>%
    left_join(metadata %>% select(c("unique_id", "type", "status")) , by = "unique_id") %>% # nolint
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "emissions_coverage")) %>%
    dplyr::mutate(emissions_coverage = as.numeric(.data$emissions_coverage) / 100) %>%
    filter(!(is.na(.data$emissions_coverage)))

  priceAprilFirst <- rawInfo  %>%
    left_join(wbRegionMapping, by = "unique_id")  %>%
    left_join(wbSectoralMapping, by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "sector_group", "price_on_1_april")) %>%
    tidyr::extract(
      .data$price_on_1_april,
      into = c("price_on_1_april_US", "price_on_1_april_local_currency"),
      regex = "US\\$\\s*([0-9.,]+) \\(([^)]*)\\)",
      remove = TRUE
    ) %>%
    select(c("unique_id", "region", "region_type", "type", "status", "sector_group", "price_on_1_april_US")) %>%
    filter(!(is.na(.data$price_on_1_april_US)))

  wbRevenue <- rawRevenue %>%
    dplyr::select(c("instrument_name",
                    all_of(names(rawRevenue)[suppressWarnings(!is.na(as.numeric(names(rawRevenue))))]))) %>%
    dplyr::left_join(metadata %>% dplyr::select(c("unique_id", "instrument_name")), by = "instrument_name") %>% # nolint
    left_join(wbRegionMapping, by = "unique_id") %>%
    left_join(wbSectoralMapping, by = "unique_id") %>%
    left_join(metadata %>% select(c("unique_id", "type", "status")), by = "unique_id") %>% # nolint
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "sector_group",
                    all_of(names(rawRevenue)[suppressWarnings(!is.na(as.numeric(names(rawRevenue))))]))) %>%
    pivot_longer(-c("unique_id", "region", "region_type", "type", "status", "sector_group"),
                 names_to = "period", values_to = "value") %>%
    filter(!(is.na(.data$value)))

  wbPrice <- rawPrice %>%
    dplyr::select(c("unique_id", all_of(names(rawPrice)[suppressWarnings(!is.na(as.numeric(names(rawPrice))))]))) %>%
    left_join(wbRegionMapping, by = "unique_id") %>%
    left_join(wbSectoralMapping, by = "unique_id") %>%
    left_join(metadata %>% select(c("unique_id", "type", "status")), by = "unique_id") %>% # nolint
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "sector_group",
                    all_of(names(rawPrice)[suppressWarnings(!is.na(as.numeric(names(rawPrice))))]))) %>%
    pivot_longer(-c("unique_id", "region", "region_type", "type", "status", "sector_group"),
                 names_to = "period", values_to = "value") %>%
    filter(!(is.na(.data$value)))

  wbEmissionsShare <- rawEmissions %>%
    dplyr::select(c("name_of_the_initiative",
                    all_of(names(rawEmissions)[suppressWarnings(!is.na(as.numeric(names(rawEmissions))))]))) %>%
    dplyr::left_join(metadata %>% dplyr::select(c("unique_id", "instrument_name"))
                     , by = join_by("name_of_the_initiative" == "instrument_name")) %>%
    left_join(wbRegionMapping, by = "unique_id") %>%
    left_join(wbSectoralMapping, by = "unique_id") %>%
    left_join(metadata %>% select(c("unique_id", "type", "status")), by = "unique_id") %>%  # nolint
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "sector_group",
                    all_of(names(rawEmissions)[suppressWarnings(!is.na(as.numeric(names(rawEmissions))))]))) %>%
    pivot_longer(-c("unique_id", "region", "region_type", "type", "status", "sector_group"),
                 names_to = "period", values_to = "value") %>%
    filter(!(is.na(.data$value)))

  # raw emissions data to calculate covered emissions per country
  histEmiRaw <- quitte::as.quitte(madrat::readSource("EDGARghg")) %>%
    select(c("region", "period", "variable", "pollutant", "value"))
  histEmiSectorRaw <- histEmiRaw %>%
    dplyr::bind_rows( # repeat last year for missing years
      histEmiRaw %>% filter(.data$period == 2023) %>% mutate(period = 2024),
      histEmiRaw %>% filter(.data$period == 2023) %>% mutate(period = 2025)
    ) %>%
    dplyr::group_by(.data$region, .data$period, .data$variable) %>%
    dplyr::summarize(value = sum(.data$value, na.rm = TRUE), .groups = "drop")
  histEmiGlo <- histEmiSectorRaw %>%
    dplyr::group_by(.data$period) %>%
    dplyr::summarize(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
    mutate(period = as.character(.data$period))
  wbEmissionsCovered <- wbEmissionsShare %>%
    left_join(histEmiGlo, by = "period", suffix = c("_share", "_emiGLO")) %>%
    mutate(value = .data$value_share * .data$value_emiGLO) %>%
    select(-c("value_share", "value_emiGLO"))

  # Subnational ("within_country") instruments mapped to their PARENT country, with the
  # instrument identity (unique_id) kept so callers can pair price x covered per
  # instrument and aggregate (calcCarbonPrice's includeSubnational path). Returned early
  # so the country-level subtypes below are completely unaffected (byte-identical).
  if (subtype %in% c("priceSubnational", "emissionsCoveredSubnational")) {
    src <- if (subtype == "priceSubnational") wbPrice else wbEmissionsCovered
    sub <- src %>%
      dplyr::filter(.data$region_type == "within_country") %>%
      dplyr::select(c("region", "unique_id", "sector_group", "period", "value"))
    return(as.magpie(sub, spatial = "region", temporal = "period", datacol = "value"))
  }

  switch(subtype,
         "price" = { dd <- wbPrice }, # nolint
         "revenue" = { dd <- wbRevenue }, # nolint
         "wbCoverage" = { dd <- wbCoverage }, # nolint
         "priceAprilFirst" = { dd <- priceAprilFirst }, # nolint
         "emissionsCovered" = { dd <- wbEmissionsCovered }) # nolint

  #expand EU ETS data to all EU countries
  EU27 <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", # nolint
            "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
  EU_ETS <- c(EU27, "ISL", "LIE", "NOR") # nolint

  if (subtype %in% c("price", "revenue", "priceAprilFirst")) {
    # EU ETS price = country ETS price
    data <- bind_rows(
      dd %>% dplyr::filter(.data$region_type == "country"),
      dd %>%
        dplyr::filter(.data$unique_id == "ETS_EU") %>%
        dplyr::select(-c("region")) %>%
        tidyr::crossing(region = EU_ETS) %>%
        dplyr::mutate(region_type = "country") %>%
        select(all_of(names(dd)))
    )
    # if the country has a higher carbon tax applied to the bulk sectors than the ETS EU price, set it to the difference
    additionalCP <- dd %>%
      filter(.data$region %in% EU_ETS,
             .data$region_type == "country",
             .data$type == "carbon_tax",
             .data$status == "implemented",
             .data$sector_group == "bulk") %>%
      left_join(data %>%
                  filter(.data$region %in% EU_ETS,
                         .data$unique_id == "ETS_EU",
                         .data$region_type == "country",
                         .data$type == "ets",
                         .data$status == "implemented",
                         .data$sector_group == "bulk") %>%
                  rename(ets_value = .data$value) %>%
                  select("region", "period", "ets_value"),
                by = c("region", "period")) %>%
      mutate(diff = .data$value - .data$ets_value) %>%
      select(-c("value", "ets_value")) %>%
      rename(value = .data$diff) %>%
      filter(.data$value > 0)
    data <- data %>%
      filter(!((.data$region %in% EU_ETS)
               & (.data$region_type == "country")
               & (.data$type == "carbon_tax")
               & (.data$status == "implemented")
               & (.data$sector_group == "bulk"))) %>%
      rbind(additionalCP)
  } else if (subtype %in% c("emissionsCovered")) {
    # country ETS coverage proportional to country bulk emissions size
    bulk <- c("Industrial Combustion", "Power Industry", "Processes", "Fuel Exploitation")
    emiBulk <- histEmiSectorRaw %>%
      dplyr::filter(.data$variable %in% bulk) %>%
      dplyr::group_by(.data$region, .data$period) %>%
      dplyr::summarize(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(period = as.character(.data$period))
    emiBulkShare <- wbEmissionsCovered %>%
      filter(.data$unique_id == "ETS_EU") %>%
      select(where(~length(unique(.x)) > 1)) %>%
      left_join(emiBulk %>% filter(.data$region %in% EU_ETS) %>% group_by(.data$period) %>% # nolint
                  summarize(value = sum(.data$value, na.rm = TRUE), .groups = "drop"),
                by = "period", suffix = c("_emi", "_wb")) %>%
      mutate(share = .data$value_emi / .data$value_wb) %>%
      select(c("period", "share"))
    emiCalcBulk <- emiBulk %>%
      filter(.data$region %in% EU_ETS) %>%
      left_join(emiBulkShare, by = c("period")) %>%
      mutate(value = coalesce(.data$value * .data$share, 0)) %>%
      select(-c("share"))
    data <- bind_rows(
      dd %>% dplyr::filter(.data$region_type == "country"),
      emiCalcBulk %>%
        mutate(
          unique_id = "ETS_EU",
          region_type = "country",
          type = "ets",
          status = "implemented",
          sector_group = "bulk"
        ) %>%
        select(all_of(names(dd)))
    )
  }

  out <- data %>%
    dplyr::select(-c("unique_id", "region_type")) %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")

  return(out)
}
