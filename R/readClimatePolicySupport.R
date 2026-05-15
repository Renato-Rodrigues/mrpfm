#' Read public climate policy support survey data
#'
#' Reads country-level survey data on public support for climate policies from
#' two sources:
#' \itemize{
#'   \item Vlasceanu et al., 2024, *Science* — support for climate policies
#'   \item Andre et al., 2024, *Nature Climate Change* — support for government
#'     climate action
#' }
#'
#' @description
#' Values in the raw data are in percent (0–100). Conversion to the [0, 1]
#' range is performed by [`convertClimatePolicySupport()`].
#'
#' @param subtype character; `"Vlasceanu2024"` or `"Andre2024"`.
#'
#' @return A [`magpie`][magclass::magclass] object with dimensions
#'   `[iso3c, year, variable]`, values in percent [0, 100].
#'
#' @author Renato Rodrigues
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select mutate rename %>% .data
#' @importFrom magclass as.magpie getSets<-
#'
readClimatePolicySupport <- function(subtype) {
  validSubtypes <- c("Vlasceanu2024", "Andre2024")
  if (!subtype %in% validSubtypes) {
    stop(
      "Unknown subtype '", subtype, "'. Must be one of: ",
      paste(validSubtypes, collapse = ", ")
    )
  }

  # madrat sets the working directory to the source folder automatically
  path <- "."

  if (subtype == "Vlasceanu2024") {
    # File: support-policies-climate.csv
    # Columns: Entity, Code, Year, Support political action on climate
    files <- list.files(path,
      pattern = "support-policies-climate\\.csv$",
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(files) == 0) stop("Cannot find support-policies-climate.csv in: ", path)
    raw <- utils::read.csv(files[[1]], stringsAsFactors = FALSE, check.names = FALSE)

    # Identify columns
    codeCol <- grep("^Code$|^country.code$|^iso", names(raw), ignore.case = TRUE, value = TRUE)[[1]]
    yearCol <- grep("^Year$|^period$", names(raw), ignore.case = TRUE, value = TRUE)[[1]]
    valueCol <- setdiff(names(raw), c("Entity", codeCol, yearCol))[[1]]

    df <- raw %>%
      dplyr::rename(iso3c = !!codeCol, year = !!yearCol, value = !!valueCol) %>%
      dplyr::filter(
        !is.na(.data$iso3c),
        nchar(as.character(.data$iso3c)) == 3,
        !grepl("^OWID_", .data$iso3c) # remove OWID aggregates (e.g. World)
      ) %>%
      dplyr::mutate(
        year     = as.integer(.data$year),
        value    = suppressWarnings(as.numeric(.data$value)),
        variable = "Support policies climate"
      ) %>%
      dplyr::select("iso3c", "year", "variable", "value")
  } else { # Andre2024
    # File: support-political-climate-action.csv
    # Columns: Entity, Code, Year, Government action on climate
    files <- list.files(path,
      pattern = "support-political-climate-action\\.csv$",
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(files) == 0) stop("Cannot find support-political-climate-action.csv in: ", path)
    raw <- utils::read.csv(files[[1]], stringsAsFactors = FALSE, check.names = FALSE)

    codeCol <- grep("^Code$|^country.code$|^iso", names(raw), ignore.case = TRUE, value = TRUE)[[1]]
    yearCol <- grep("^Year$|^period$", names(raw), ignore.case = TRUE, value = TRUE)[[1]]
    valueCol <- setdiff(names(raw), c("Entity", codeCol, yearCol))[[1]]

    df <- raw %>%
      dplyr::rename(iso3c = !!codeCol, year = !!yearCol, value = !!valueCol) %>%
      dplyr::filter(
        !is.na(.data$iso3c),
        nchar(as.character(.data$iso3c)) == 3,
        !grepl("^OWID_", .data$iso3c) # remove OWID aggregates (e.g. World)
      ) %>%
      dplyr::mutate(
        year     = as.integer(.data$year),
        value    = suppressWarnings(as.numeric(.data$value)),
        variable = "Support political climate action"
      ) %>%
      dplyr::select("iso3c", "year", "variable", "value")
  }

  out <- df %>%
    magclass::as.magpie(spatial = "iso3c", temporal = "year", datacol = "value")

  magclass::getSets(out) <- c("iso3c", "year", "variable")

  return(out)
}
