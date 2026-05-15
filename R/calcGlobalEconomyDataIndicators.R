#' Calculate the Global Economy Data indicators including assumptions to fill all missing country data
#'
#' @param outPeriod vector of years to include in the output. Default is 2003:2021.
#'  Make sure to include years with enough data.
#' @returns MAgPIE object with Global Economy Data indicators per indicator type, indicator name, indicator unit,
#'  country and period
#'
#' @author Renato Rodrigues
#'
#' @examples
#' \dontrun{
#' calcOutput("GlobalEconomyDataIndicators", outPeriod = 2003:2021)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_wider replace_na complete
#' @importFrom dplyr arrange if_all relocate mutate left_join filter select last_col ungroup group_by .data n_distinct
#'  summarise arrange rowwise count
#' @importFrom zoo na.approx na.locf
#' @importFrom stringr str_to_lower
#' @importFrom stats weighted.mean
#'
calcGlobalEconomyDataIndicators <- function(outPeriod = 2003:2021) {

  # read the data
  raw <- readSource("GlobalEconomyData")
  rawData <- as.quitte(raw) %>%
    select(c("region", "type", "driver", "period", "value"))

  # filter the data
  indicatorsMapping <- toolGetMapping("globalEconomicDataIndicators.csv", type = "sectoral", where = "mrpfm")
  filterData <- rawData %>%
    dplyr::arrange(.data$period) %>%
    dplyr::filter(.data$period %in% outPeriod) %>% # filter to have only years with enough data
    tidyr::pivot_wider(names_from = "period", values_from = "value") %>%
    #filter(!if_all(matches("^\\d+$"), is.na)) %>% # remove rows with only nas
    #select(where(~ !all(is.na(.)))) %>% # remove columns with only nas
    dplyr::left_join(indicatorsMapping, by = c("type", "driver")) %>% # adding metadata
    dplyr::relocate(matches("^\\d+$"), .after = dplyr::last_col()) %>%
    mutate(duplicated = tidyr::replace_na(.data$duplicated, FALSE),
           na_is_zero = tidyr::replace_na(.data$na_is_zero, FALSE)) %>%
    dplyr::filter(.data$duplicated == FALSE) # Filter out rows where 'duplicated' is TRUE

  # fill NAs with other data from country
  fillLongData <- filterData %>%
    tidyr::pivot_longer(cols = dplyr::matches("^\\d+$"), names_to = "period", values_to = "value") %>%
    dplyr::mutate(period = as.numeric(as.character(.data$period)),
                  value = as.numeric(.data$value)) %>%
    dplyr::group_by(.data$type, .data$driver, .data$region) %>%
    dplyr::mutate(value = zoo::na.approx(.data$value, na.rm = FALSE), # Use linear interpolation to fill NAs
                  value = zoo::na.locf(zoo::na.locf(.data$value, na.rm = FALSE),
                                       na.rm = FALSE,
                                       fromLast = TRUE)) %>% # fill remaining NAs on extremes repeating values
    dplyr::ungroup()

  # replace NAs with zeros on specific indicators
  longData <- rbind(
    fillLongData %>%
      dplyr::filter(!(.data$na_is_zero)),
    fillLongData %>%
      dplyr::filter(.data$na_is_zero) %>%
      tidyr::complete(.data$driver, .data$period) %>%
      dplyr::select(- c("driver_name", "unit", "weight", "duplicated", "na_is_zero", "type", "source")) %>%
      dplyr::left_join(indicatorsMapping %>% dplyr::distinct(.data$driver, .keep_all = TRUE), by = c("driver")) %>% # nolint
      dplyr::relocate("type", .after = "region") %>%
      dplyr::group_by(.data$type, .data$driver) %>%
      dplyr::mutate(value = tidyr::replace_na(.data$value, 0))
  )

  # weight ("arableLand", "GDPperCapita", "landArea", "population", "NULL", "")
  mapping <- toolGetMapping("regionmapping_62.csv", where = "mappingfolder", type = "regional")

  gdp <- calcOutput("GDPPast", aggregate = FALSE)[, outPeriod, ] %>%
    as.data.frame() %>%
    select("Region", "Year", "Value") %>%
    setNames(c("region", "period", "gdp"))
  pop <- calcOutput("PopulationPast", aggregate = FALSE)[, outPeriod, ] %>%
    as.data.frame() %>%
    select("Region", "Year", "Value") %>%
    setNames(c("region", "period", "pop"))
  area <- calcOutput("LanduseInitialisation",aggregate=FALSE)[, 2010, ] # from mrlandcore # nolint

  weight <- gdp %>%
    left_join(pop, by = c("region", "period")) %>%
    mutate(period = as.numeric(as.character(.data$period)),
           gdp = as.numeric(.data$gdp),
           pop = as.numeric(.data$pop),
           gdpPerCapita = gdp / pop)

  weightVec <- c("arableLand" = "area", "landArea" = "area",
                 "population" = "pop", "gdp" = "gdp", "GDPperCapita" = "gdpPerCapita")

  longDataMap <- longData %>%
    left_join(weight, by = c("region", "period")) %>% # Merge weight data
    left_join(mapping, by = c("region" = "CountryCode")) %>% # Merge mapping
    mutate(weight = tidyr::replace_na(weightVec[stringr::str_to_lower(.data$weight)],
                                      "pop")) %>% # convert weight names to the available ones
    dplyr::rowwise() %>%
    dplyr::mutate(weight_val = get(weight)) %>% # Create a single numeric "weight_val" column
    dplyr::ungroup()

  # Compute regional weighted averages and mean weights
  regionalAvg <- longDataMap %>%
    dplyr::filter(!is.na(.data$value)) %>%
    group_by(.data$RegionCode, .data$driver, .data$driver_name, .data$unit, .data$weight, .data$period) %>%
    dplyr::summarise(
      reg_avg = stats::weighted.mean(.data$value, .data$weight_val, na.rm = TRUE),
      reg_weight_sum = sum(.data$weight_val, na.rm = TRUE),  # Changed to sum
      .groups = "drop"
    )

  # Join back and impute
  data <- longDataMap %>%
    left_join(regionalAvg,
              by = c("RegionCode", "driver", "driver_name", "unit", "weight", "period")) %>%
    mutate(
      # Detect percentage, per capita, ratios, intensity, or share
      is_percent_like = stringr::str_detect(stringr::str_to_lower(.data$unit), "%|per capita|index|ratio|boolean"),
      value_filled = case_when(
        !is.na(.data$value) ~ .data$value,                                   # keep existing values
        .data$is_percent_like ~ .data$reg_avg,                               # no scaling for relative indicators
        TRUE ~ .data$reg_avg * (.data$weight_val / .data$reg_weight_sum)     # scale absolute measures
      ),
      fill_type = case_when(
        !is.na(.data$value) ~ "original",
        .data$is_percent_like ~ "filled_no_scaling",
        TRUE ~ "filled_scaled"
      )
    ) %>%
    select("region", "type", "driver", "driver_name", "unit", "weight", "period",
           value_original = "value", "value_filled", "is_percent_like", "fill_type", "weight_val")

  # 6️⃣ Summary output for debugging
  #fillSummary <- data %>% # nolint
  #  dplyr::count(.data$fill_type) %>% # nolint
  #  dplyr::mutate(pct = round(100 * .data$n / sum(.data$n), 1)) # nolint
  #print(fillSummary) # nolint

  outData <- data %>%
    dplyr::select("region", "type", "driver_name", "unit", "period", value = "value_filled") %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")

  outWeight <- data %>%
    dplyr::select("region", "type", "driver_name", "unit", "period", value = "weight_val") %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")

  return(list(x = outData,
              weight = outWeight,
              description = "Global economy data indicators",
              unit = "unit described in the variable"))
}
