#' Read OECD Climate Actions and Policies Measurement Framework (CAPMF) data
#'
#' Reads the CAPMF policy stringency data from a CSV file.
#'
#' @param subtype character, type of data: "all" or specific sector.
#' @returns A [`magpie`][magclass::magclass] object with dimensions `[iso3c, year, variable]`.
#'
#' @author Renato Rodrigues
#'
#' @importFrom utils read.csv
#' @importFrom dplyr select all_of matches mutate left_join across filter %>% rename_with group_by summarize
#' @importFrom countrycode countrycode
#' @importFrom magclass as.magpie getSets<-
#' @importFrom madrat toolCountryFill
#'
#' @export
readCAPMF <- function(subtype = "all") {
  if (subtype == "expanded") {
    csvFile <- "capmf_expanded.csv"
    if (!file.exists(csvFile)) {
      # Try case-insensitive listing
      files <- list.files(path = ".", pattern = "capmf_expanded.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
      if (length(files) > 0) {
        csvFile <- files[[1]]
      } else {
        # Fallback to capmf.csv if expanded not found
        files_all <- list.files(path = ".", pattern = "capmf.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
        if (length(files_all) == 0) stop("No CSV file found in CAPMF source folder. Expected e.g. capmf.csv")
        csvFile <- files_all[[1]]
      }
    }
  } else {
    # Exclude capmf_expanded.csv when reading standard CAPMF
    files <- list.files(path = ".", pattern = "capmf.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
    files <- files[!grepl("capmf_expanded", files, ignore.case = TRUE)]
    if (length(files) == 0) {
      files <- list.files(path = ".", pattern = "capmf.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
    }
    if (length(files) == 0) stop("No CSV file found in CAPMF source folder. Expected e.g. capmf.csv")
    csvFile <- files[[1]]
  }

  raw <- utils::read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)

  # Standardize column names to lowercase for robust matching
  colnames(raw) <- tolower(colnames(raw))

  if ("measure" %in% colnames(raw)) {
    raw <- raw[toupper(raw$measure) == "POL_STRINGENCY", ]
  }

  # Identify Country / Spatial column
  if ("ref_area" %in% colnames(raw)) {
    raw$iso3c <- raw$ref_area
  } else if ("country" %in% colnames(raw)) {
    raw$iso3c <- raw$country
  } else if ("cou" %in% colnames(raw)) {
    raw$iso3c <- raw$cou
  } else if ("spatial" %in% colnames(raw)) {
    raw$iso3c <- raw$spatial
  } else {
    stop("Could not find country code column (ref_area, country, cou, or spatial) in CAPMF CSV.")
  }

  # Identify Year / Temporal column
  if ("time_period" %in% colnames(raw)) {
    raw$year <- raw$time_period
  } else if ("year" %in% colnames(raw)) {
    raw$year <- raw$year
  } else if ("period" %in% colnames(raw)) {
    raw$year <- raw$period
  } else if ("temporal" %in% colnames(raw)) {
    raw$year <- raw$temporal
  } else {
    stop("Could not find year column (time_period, year, period, or temporal) in CAPMF CSV.")
  }

  # Identify Value / Stringency column
  if ("obs_value" %in% colnames(raw)) {
    raw$value <- raw$obs_value
  } else if ("value" %in% colnames(raw)) {
    raw$value <- raw$value
  } else if ("stringency" %in% colnames(raw)) {
    raw$value <- raw$stringency
  } else {
    stop("Could not find value column (obs_value, value, or stringency) in CAPMF CSV.")
  }

  # Identify Sector / Subject / Activity / Policy column
  if ("clim_act_pol" %in% colnames(raw)) {
    raw$sector <- raw$clim_act_pol
  } else if ("sector" %in% colnames(raw)) {
    raw$sector <- raw$sector
  } else if ("sector_group" %in% colnames(raw)) {
    raw$sector <- raw$sector_group
  } else if ("subject" %in% colnames(raw)) {
    raw$sector <- raw$subject
  } else if ("activity" %in% colnames(raw)) {
    raw$sector <- raw$activity
  } else {
    stop("Could not find sector/policy column (clim_act_pol, sector, sector_group, subject, or activity) in CAPMF CSV.")
  }

  # Clean values and map country codes to ISO 3166-1 alpha-3
  raw$iso3c <- countrycode::countrycode(
    raw$iso3c,
    origin = "iso3c", destination = "iso3c", warn = FALSE
  )

  raw <- raw[!is.na(raw$iso3c), ]

  # Standardize sector names to variable labels
  raw$variable <- paste0(raw$sector, " (CAPMF)")

  df <- raw %>%
    dplyr::select(c("iso3c", "year", "variable", "value")) %>%
    dplyr::mutate(
      year = as.integer(.data$year),
      value = as.numeric(.data$value)
    ) %>%
    dplyr::filter(!is.na(.data$value))

  # Average duplicate entries (e.g. if the CSV contains granular sub-policies per sector)
  df <- df %>%
    dplyr::group_by(.data$iso3c, .data$year, .data$variable) %>%
    dplyr::summarize(value = mean(.data$value, na.rm = TRUE), .groups = "drop")

  out <- magclass::as.magpie(df, spatial = "iso3c", temporal = "year", datacol = "value")
  magclass::getSets(out) <- c("iso3c", "year", "variable")
  out <- madrat::toolCountryFill(out, fill = NA, verbosity = 0)
  return(out)
}
