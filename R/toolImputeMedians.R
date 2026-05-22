#' Impute missing values with regional or global medians
#'
#' @description
#' Fills NA values in a magpie object by assigning the median of the region the country belongs to.
#' It evaluates multiple region mapping files in order of priority. If a mapping cannot resolve a non-missing
#' regional median, it tries the next mapping file, eventually falling back to the global median of the year and variable.
#'
#' @param data A country-level magpie object to be imputed.
#' @param regionMappingFiles A vector of names of the region mapping files in order of priority.
#'                           Defaults to `c("regionmapping_54.csv", "regionmappingH12.csv")`.
#'
#' @return An imputed magpie object of the same shape.
#' @author Renato Rodrigues
#'
#' @importFrom magclass getItems getYears getNames
#' @importFrom madrat toolGetMapping
#' @importFrom stats median
#' @export
toolImputeMedians <- function(data, regionMappingFiles = c("regionmapping_54.csv", "regionmappingH12.csv")) {
  # If there are no NAs, return immediately
  if (!any(is.na(data))) {
    return(data)
  }

  # Load all valid mapping files and cache their country-to-region lookups
  mappingsInfo <- list()
  for (mapFile in regionMappingFiles) {
    mapping <- tryCatch(
      toolGetMapping(mapFile, type = "regional", where = "mappingfolder"),
      error = function(e) {
        warning("Could not load region mapping file: ", mapFile)
        NULL
      }
    )
    if (!is.null(mapping)) {
      if (!all(c("CountryCode", "RegionCode") %in% colnames(mapping))) {
        warning("Mapping file ", mapFile, " does not contain 'CountryCode' and 'RegionCode' columns. Skipping.")
        next
      }
      country2region <- setNames(mapping$RegionCode, mapping$CountryCode)
      mappingsInfo[[mapFile]] <- list(
        country2region = country2region
      )
    }
  }

  if (length(mappingsInfo) == 0) {
    warning("No valid region mappings loaded. Falling back entirely to global medians.")
  }

  out <- data
  countries <- getItems(data, dim = 1)
  years <- getYears(data)
  vars <- getNames(data)

  # Loop through years and variables
  for (v in vars) {
    for (y in years) {
      vals <- out[, y, v]
      naIdx <- which(is.na(vals))
      if (length(naIdx) == 0) next

      # Precompute non-NA countries for this specific year/variable
      nonNaVals <- vals[-naIdx, , ]
      nonNaCountries <- getItems(nonNaVals, dim = 1)

      # 1. Compute global median
      globalMedian <- if (length(nonNaCountries) > 0) {
        median(as.numeric(vals), na.rm = TRUE)
      } else {
        NA_real_
      }

      # 2. Precompute regional medians for all active mapping files
      regMediansPerMap <- list()
      for (mapFile in names(mappingsInfo)) {
        country2region <- mappingsInfo[[mapFile]]$country2region
        nonNaRegions <- country2region[nonNaCountries]

        regMedians <- list()
        if (length(nonNaCountries) > 0) {
          # Find unique regions that have data
          uniqueRegs <- unique(nonNaRegions[!is.na(nonNaRegions)])
          for (r in uniqueRegs) {
            rCountries <- nonNaCountries[which(nonNaRegions == r)]
            regMedians[[r]] <- median(as.numeric(vals[rCountries, , ]), na.rm = TRUE)
          }
        }
        regMediansPerMap[[mapFile]] <- regMedians
      }

      # 3. Impute missing values
      naCountries <- countries[naIdx]
      for (i in seq_along(naIdx)) {
        cnt <- naCountries[i]

        imputedVal <- NA_real_
        # Try each mapping file in the order provided
        for (mapFile in names(mappingsInfo)) {
          country2region <- mappingsInfo[[mapFile]]$country2region
          regMedians <- regMediansPerMap[[mapFile]]

          reg <- country2region[cnt]
          if (!is.na(reg) && reg %in% names(regMedians)) {
            val <- regMedians[[reg]]
            if (!is.na(val)) {
              imputedVal <- val
              break # Found a valid regional median! Stop trying other mappings.
            }
          }
        }

        # 4. Fallback to global median if all regional medians are NA or unresolved
        if (is.na(imputedVal)) {
          imputedVal <- globalMedian
        }

        # If we successfully found a median, assign it
        if (!is.na(imputedVal)) {
          out[naIdx[i], y, v] <- imputedVal
        }
      }
    }
  }

  return(out)
}
