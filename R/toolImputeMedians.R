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
#' @importFrom magclass getItems getYears getNames getSets getSets<- as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom stats median aggregate
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
      country2region <- stats::setNames(mapping$RegionCode, mapping$CountryCode)
      mappingsInfo[[mapFile]] <- list(
        country2region = country2region
      )
    }
  }

  if (length(mappingsInfo) == 0) {
    warning("No valid region mappings loaded. Falling back entirely to global medians.")
  }

  df <- as.data.frame(data)

  # Identify standard columns in magclass data.frame
  reg_col <- if ("Region" %in% colnames(df)) "Region" else if ("Cell" %in% colnames(df)) "Cell" else colnames(df)[1]
  yr_col  <- if ("Year" %in% colnames(df)) "Year" else colnames(df)[2]
  val_col <- if ("Value" %in% colnames(df)) "Value" else colnames(df)[4]
  # All data-subdimension columns (everything that is not region / year / value). Combining
  # them into a single variable key makes the imputation robust to a multi-subdimension 3rd
  # dimension (e.g. Scenario.Variable in calcSSPextensions); single-subdim callers (WGI, VDem)
  # reduce to the prior single-"Data1" behaviour.
  dim_cols <- setdiff(colnames(df), c("Cell", reg_col, yr_col, val_col))

  df$Region_char <- as.character(df[[reg_col]])
  df$Year_char   <- as.character(df[[yr_col]])
  df$Var_char    <- if (length(dim_cols) == 1) as.character(df[[dim_cols]]) else
    do.call(paste, c(df[dim_cols], sep = "."))
  df$Val_num     <- as.numeric(df[[val_col]])

  # 1. Compute global medians by Year and Variable
  global_medians <- aggregate(Val_num ~ Year_char + Var_char, data = df, FUN = median, na.rm = TRUE)
  global_medians$Key_Global <- paste(global_medians$Year_char, global_medians$Var_char, sep = "_")
  global_medians_vec <- stats::setNames(global_medians$Val_num, global_medians$Key_Global)

  # 2. Precompute regional medians for all active mapping files
  regMediansVecPerMap <- list()
  for (mapFile in names(mappingsInfo)) {
    country2region <- mappingsInfo[[mapFile]]$country2region

    df$RegionCode <- country2region[df$Region_char]

    # Compute medians by RegionCode, Year, Var
    reg_medians <- aggregate(Val_num ~ RegionCode + Year_char + Var_char, data = df, FUN = median, na.rm = TRUE)
    reg_medians$Key_Region <- paste(reg_medians$Year_char, reg_medians$Var_char, reg_medians$RegionCode, sep = "_")

    regMediansVecPerMap[[mapFile]] <- stats::setNames(reg_medians$Val_num, reg_medians$Key_Region)
  }

  # 3. Impute missing values (Fully Vectorized)
  imputedVal <- df$Val_num
  na_idx <- which(is.na(imputedVal))

  if (length(na_idx) > 0) {
    na_regions <- df$Region_char[na_idx]
    na_years   <- df$Year_char[na_idx]
    na_vars    <- df$Var_char[na_idx]

    # Try each mapping in order of priority
    for (mapFile in names(mappingsInfo)) {
      country2region <- mappingsInfo[[mapFile]]$country2region
      reg_medians_vec <- regMediansVecPerMap[[mapFile]]

      region_vec <- country2region[na_regions]
      lookup_key <- paste(na_years, na_vars, region_vec, sep = "_")

      cntMedians <- reg_medians_vec[lookup_key]

      still_na <- is.na(imputedVal[na_idx])
      imputedVal[na_idx][still_na] <- cntMedians[still_na]
    }

    # 4. Fallback to global median if all regional medians are NA or unresolved
    lookup_key_global <- paste(na_years, na_vars, sep = "_")
    globalMedians <- global_medians_vec[lookup_key_global]

    still_na <- is.na(imputedVal[na_idx])
    imputedVal[na_idx][still_na] <- globalMedians[still_na]

    df[[val_col]] <- imputedVal
  }

  # 5. Convert back to magpie object and restore sets
  out <- as.magpie(df[, c(reg_col, yr_col, dim_cols, val_col)],
                   spatial = 1, temporal = 2, datacol = 2 + length(dim_cols) + 1)
  getSets(out) <- getSets(data)

  return(out)
}
