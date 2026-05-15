#' Read SSP Extensions data
#'
#' Read the input data file for the ssp extensions data
#'
#' @return MagPIE object
#' @author Renato Rodrigues
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom magclass as.magpie
#' @importFrom madrat toolCountry2isocode
#' @importFrom dplyr matches %>% filter bind_rows mutate select anti_join
#'
readSSPextensions <- function() {
  file <- "ssp-extensions_all_data.xlsx"

  data <- readxl::read_excel(file, col_types = "text") %>%
    mutate(across(matches("^\\d{4}$"), as.numeric)) %>%
    tidyr::pivot_longer(cols = matches("^\\d{4}$"), names_to = "period", values_to = "value") %>%
    dplyr::filter(
      .data$Region != "Kosovo", # Remove Kosovo (not in REMIND regional mapping)
      .data$Scenario != "WDI" # Remove WDI scenario as it is only 1s and 0s
    ) %>%
    dplyr::mutate(
      Variable = dplyr::case_when(
        .data$Variable == "Population|Urban [Share]" &
          .data$Model == "Urbanization Model (UNDP, 2022)" ~ "Population|Urban UNDP [Share]",
        TRUE ~ .data$Variable
      )
    ) %>%
    dplyr::select(-Model)

  # Copy Observed data to SSP scenarios
  observed <- data %>%
    dplyr::filter(.data$Scenario == "Observed" & !is.na(.data$value))

  others <- data %>%
    dplyr::filter(.data$Scenario != "Observed")

  if (nrow(observed) > 0) {
    scenarios <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
    # Duplicate observed data for each target scenario
    expandedObserved <- lapply(scenarios, function(s) {
      observed %>% dplyr::mutate(Scenario = s)
    }) %>%
      dplyr::bind_rows()

    # Combine original data (excluding Observed to avoid duplication)
    # If a value exists in observed and ssp, use the observed one (anti_join others with expandedObserved)
    others <- others %>%
      dplyr::anti_join(expandedObserved, by = c("Region", "Variable", "period", "Scenario"))

    data <- dplyr::bind_rows(others, expandedObserved)
  }

  # transfer into ISO country names
  data <- data %>%
    mutate(Region = toolCountry2isocode(data$Region))

  out <- data %>%
    magclass::as.magpie(spatial = "Region", temporal = "period", datacol = "value")

  return(out)
}
